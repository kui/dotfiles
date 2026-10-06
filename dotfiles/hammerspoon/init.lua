hs.loadSpoon("EmmyLua")

-- `hs` コマンドを ~/.local/bin に入れる（ターミナルから `hs -c 'hs.reload()'` などで操作できる）
require("hs.ipc")
hs.ipc.cliInstall(os.getenv("HOME") .. "/.local")

-- github.com/kui/kui-ahk と同等の挙動を macOS で再現する設定
--   * F18 をモディファイアキーとした第2レイヤー（未定義キーは無効化）
--   * IME 切り替え時の全画面中央オーバーレイ
--   * 日本語入力中のマウスカーソル付近インジケーター（タイピング中は非表示）
--   * マウスが一定距離動いたら英数入力に自動切り替え

local function log(message)
    hs.console.printStyledtext(os.date("[%Y-%m-%d %H:%M:%S] ") .. message)
end

local MODIFIER_KEYS = {"shift", "cmd", "alt", "ctrl", "fn"}

-- 想定している入力ソース
-- それぞれ先にあるものが優先される
local INPUT_SOURCES = {
    -- 英数入力モード
    ROMAN = {"com.google.inputmethod.Japanese.Roman", "com.apple.inputmethod.Kotoeri.RomajiTyping.Roman",
             "com.apple.keylayout.ABC"},
    -- 日本語入力モード
    JAPANESE = {"com.google.inputmethod.Japanese.base", "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese"}
}

local function contains(array, value)
    for _, item in ipairs(array) do
        if item == value then
            return true
        end
    end
    return false
end

-- ========================================
-- IME 制御と表示
-- ========================================

local IME_CONFIG = {
    FONT = "HiraginoSans-W6",
    COLOR = {
        JAPANESE = {
            hex = "#4CAF50"
        },
        ROMAN = {
            hex = "#2196F3"
        },
        TEXT = {
            white = 1.0,
            alpha = 1.0
        }
    },
    RADIUS = 5,
    -- 画面中央に表示するオーバーレイ
    OVERLAY = {
        WIDTH = 120,
        HEIGHT = 80,
        TEXT_SIZE = 48,
        DURATION = 1.0 -- 表示時間（秒）
    },
    -- マウスカーソル付近のインジケーター
    MOUSE_INDICATOR = {
        WIDTH = 50,
        HEIGHT = 35,
        TEXT_SIZE = 20,
        OFFSET = 20, -- マウスカーソルからのオフセット
        UPDATE_INTERVAL = 0.05, -- マウス追従の更新間隔（秒）
        RESTORE_DELAY = 3.0 -- タイピング停止後にインジケーターを復活させるまでの時間（秒）
    },
    -- この距離（px）以上マウスが動いたら英数入力に切り替える
    MOUSE_MOVE_THRESHOLD = 500
}

-- GC されないようにグローバルで保持する
ImeState = {
    japanese = false,
    ---@type hs.geometry|nil 英数切り替え判定用の基準マウス位置
    anchor = nil,
    -- タイピング中にマウスインジケーターを非表示にしているか
    suppressed = false,
    ---@type hs.canvas|nil
    mouseIndicator = nil,
    ---@type hs.timer|nil
    mouseTimer = nil,
    ---@type hs.timer|nil
    restoreTimer = nil,
    ---@type hs.canvas[]
    overlays = {},
    ---@type hs.timer|nil
    overlayTimer = nil,
    ---@type hs.timer|nil
    switchTimer = nil
}

local function isJapaneseSource(sourceID)
    return contains(INPUT_SOURCES.JAPANESE, sourceID)
end

-- テキストを中央に配置したラベル canvas を作成
local function createLabel(frame, text, textSize, bgColor)
    local styled = hs.styledtext.new(text, {
        font = {
            name = IME_CONFIG.FONT,
            size = textSize
        },
        color = IME_CONFIG.COLOR.TEXT,
        paragraphStyle = {
            alignment = "center"
        }
    })
    local textHeight = hs.drawing.getTextDrawingSize(styled).h

    local canvas = hs.canvas.new(frame)
    ---@cast canvas hs.canvas
    canvas:appendElements({
        type = "rectangle",
        action = "fill",
        fillColor = bgColor,
        roundedRectRadii = {
            xRadius = IME_CONFIG.RADIUS,
            yRadius = IME_CONFIG.RADIUS
        }
    }, {
        type = "text",
        text = styled,
        frame = {
            x = 0,
            y = (frame.h - textHeight) / 2,
            w = frame.w,
            h = textHeight
        }
    })
    canvas:level("overlay")
    canvas:behavior({"canJoinAllSpaces", "stationary"})
    return canvas
end

-- IME 状態を全画面の中央に一時表示
local function showCenterOverlay(japanese)
    for _, overlay in ipairs(ImeState.overlays) do
        overlay:delete()
    end
    ImeState.overlays = {}
    if ImeState.overlayTimer then
        ImeState.overlayTimer:stop()
    end

    local cfg = IME_CONFIG.OVERLAY
    for _, screen in ipairs(hs.screen.allScreens()) do
        local sf = screen:frame()
        local overlay = createLabel({
            x = sf.x + (sf.w - cfg.WIDTH) / 2,
            y = sf.y + (sf.h - cfg.HEIGHT) / 2,
            w = cfg.WIDTH,
            h = cfg.HEIGHT
        }, japanese and "あ" or "_A", cfg.TEXT_SIZE, japanese and IME_CONFIG.COLOR.JAPANESE or IME_CONFIG.COLOR.ROMAN)
        overlay:show()
        table.insert(ImeState.overlays, overlay)
    end

    ImeState.overlayTimer = hs.timer.doAfter(cfg.DURATION, function()
        for _, overlay in ipairs(ImeState.overlays) do
            overlay:delete()
        end
        ImeState.overlays = {}
    end)
end

-- マウスカーソル付近のインジケーター位置を更新
local function updateMouseIndicatorPosition()
    local indicator = ImeState.mouseIndicator
    if not indicator then
        return
    end
    local cfg = IME_CONFIG.MOUSE_INDICATOR
    local mousePos = hs.mouse.absolutePosition()
    local screen = hs.mouse.getCurrentScreen()
    if not screen then
        return
    end
    local sf = screen:fullFrame()

    -- デフォルトは右下
    local x = mousePos.x + cfg.OFFSET
    local y = mousePos.y + cfg.OFFSET
    -- 右端に近い場合は左側に表示
    if x + cfg.WIDTH > sf.x + sf.w then
        x = mousePos.x - cfg.WIDTH - cfg.OFFSET
    end
    -- 下端に近い場合は上側に表示
    if y + cfg.HEIGHT > sf.y + sf.h then
        y = mousePos.y - cfg.HEIGHT - cfg.OFFSET
    end

    indicator:topLeft({
        x = x,
        y = y
    })
end

-- IME 状態とタイピング状態に応じてマウスインジケーターの表示を切り替え
local function updateMouseIndicatorVisibility()
    if ImeState.japanese and not ImeState.suppressed then
        if not ImeState.mouseIndicator then
            local cfg = IME_CONFIG.MOUSE_INDICATOR
            ImeState.mouseIndicator = createLabel({
                x = 0,
                y = 0,
                w = cfg.WIDTH,
                h = cfg.HEIGHT
            }, "あ", cfg.TEXT_SIZE, IME_CONFIG.COLOR.JAPANESE)
        end
        updateMouseIndicatorPosition()
        ImeState.mouseIndicator:show()
    elseif ImeState.mouseIndicator then
        ImeState.mouseIndicator:hide()
    end
end

local setIme

-- 日本語入力中: マウスが基準位置から閾値以上離れたら英数に切り替え、そうでなければインジケーターを追従
local function onMouseTick()
    local pos = hs.mouse.absolutePosition()
    local anchor = ImeState.anchor or pos
    local dx, dy = pos.x - anchor.x, pos.y - anchor.y
    if math.sqrt(dx * dx + dy * dy) > IME_CONFIG.MOUSE_MOVE_THRESHOLD then
        setIme(false)
    elseif not ImeState.suppressed then
        updateMouseIndicatorPosition()
    end
end

-- 内部状態を IME 状態に合わせる
local function applyImeState(japanese)
    ImeState.japanese = japanese
    if japanese then
        if not ImeState.mouseTimer then
            ImeState.mouseTimer = hs.timer.doEvery(IME_CONFIG.MOUSE_INDICATOR.UPDATE_INTERVAL, onMouseTick)
        end
    elseif ImeState.mouseTimer then
        ImeState.mouseTimer:stop()
        ImeState.mouseTimer = nil
    end
    updateMouseIndicatorVisibility()
end

-- JIS キーボードの「英数」「かな」キー
local EISU_KEYCODE = 102
local KANA_KEYCODE = 104

-- TISSelectInputSource（hs.keycodes.currentSourceID）でキーボードレイアウト（ABC）から
-- IME に切り替えると、メニューバーの表示だけ変わって実際の入力が切り替わらないことがある。
-- そのため「英数」「かな」キーを送出して IME 自身に切り替えさせ、効かなかった場合のみ TIS で切り替える
local function switchInputSource(japanese)
    hs.eventtap.keyStroke({}, japanese and KANA_KEYCODE or EISU_KEYCODE, 0)
    -- 入力ソースの変更は非同期に反映されるので、少し待ってから確認する
    if ImeState.switchTimer then
        ImeState.switchTimer:stop()
    end
    ImeState.switchTimer = hs.timer.doAfter(0.3, function()
        ImeState.switchTimer = nil
        if isJapaneseSource(hs.keycodes.currentSourceID()) == japanese then
            return
        end
        for _, sourceID in ipairs(japanese and INPUT_SOURCES.JAPANESE or INPUT_SOURCES.ROMAN) do
            if hs.keycodes.currentSourceID(sourceID) then
                return
            end
        end
    end)
end

-- 明示的な IME 切り替え（F18+Space やマウス移動による自動切り替え）
setIme = function(japanese)
    ImeState.suppressed = false
    switchInputSource(japanese)
    showCenterOverlay(japanese)
    ImeState.anchor = hs.mouse.absolutePosition()
    applyImeState(japanese)
end

-- 入力ソースが外部で変更された場合
local function onInputSourceChanged()
    local japanese = isJapaneseSource(hs.keycodes.currentSourceID())
    if japanese ~= ImeState.japanese then
        ImeState.suppressed = false
        ImeState.anchor = hs.mouse.absolutePosition()
    end
    applyImeState(japanese)
end

-- キー入力時にマウスインジケーターを一時的に非表示にする
local function onTyping()
    if not ImeState.japanese then
        return
    end
    if not ImeState.suppressed then
        ImeState.suppressed = true
        updateMouseIndicatorVisibility()
    end
    -- タイピング停止後に復活（キー入力のたびにリセット）
    if ImeState.restoreTimer then
        ImeState.restoreTimer:stop()
    end
    ImeState.restoreTimer = hs.timer.doAfter(IME_CONFIG.MOUSE_INDICATOR.RESTORE_DELAY, function()
        if ImeState.suppressed and ImeState.japanese then
            ImeState.suppressed = false
            updateMouseIndicatorVisibility()
        end
    end)
end

-- ========================================
-- F18 モディファイアキー
-- ========================================

-- F18キーとの組み合わせで変換するキーマップ
local f18Keymap = {{
    trigger = {
        key = "h"
    },
    action = {
        key = "left"
    }
}, {
    trigger = {
        key = "j"
    },
    action = {
        key = "down"
    }
}, {
    trigger = {
        key = "k"
    },
    action = {
        key = "up"
    }
}, {
    trigger = {
        key = "l"
    },
    action = {
        key = "right"
    }
}, {
    -- 行頭へジャンプ
    trigger = {
        key = "h",
        mods = {"alt"}
    },
    action = {
        key = "left",
        mods = {"cmd"}
    }
}, {
    -- 最下部へジャンプ
    trigger = {
        key = "j",
        mods = {"alt"}
    },
    action = {
        key = "down",
        mods = {"cmd"}
    }
}, {
    -- 最上部へジャンプ
    trigger = {
        key = "k",
        mods = {"alt"}
    },
    action = {
        key = "up",
        mods = {"cmd"}
    }
}, {
    -- 行末へジャンプ
    trigger = {
        key = "l",
        mods = {"alt"}
    },
    action = {
        key = "right",
        mods = {"cmd"}
    }
}, {
    trigger = {
        key = "h",
        mods = {"cmd"}
    },
    action = {
        key = "left",
        mods = {"cmd"}
    }
}, {
    trigger = {
        key = "j",
        mods = {"cmd"}
    },
    action = {
        key = "down",
        mods = {"cmd"}
    }
}, {
    trigger = {
        key = "k",
        mods = {"cmd"}
    },
    action = {
        key = "up",
        mods = {"cmd"}
    }
}, {
    trigger = {
        key = "l",
        mods = {"cmd"}
    },
    action = {
        key = "right",
        mods = {"cmd"}
    }
}, {
    -- 単語ジャンプ左
    trigger = {
        key = "h",
        mods = {"ctrl"}
    },
    action = {
        key = "left",
        mods = {"alt"}
    }
}, {
    trigger = {
        key = "j",
        mods = {"ctrl"}
    },
    action = {
        key = "pagedown"
    }
}, {
    trigger = {
        key = "k",
        mods = {"ctrl"}
    },
    action = {
        key = "pageup"
    }
}, {
    -- 単語ジャンプ右
    trigger = {
        key = "l",
        mods = {"ctrl"}
    },
    action = {
        key = "right",
        mods = {"alt"}
    }
}, {
    -- Delete
    trigger = {
        key = "s"
    },
    action = {
        key = "delete"
    }
}, {
    -- Forward Delete
    trigger = {
        key = "d"
    },
    action = {
        key = "forwarddelete"
    }
}, {
    -- キャレット左から行頭までを選択
    trigger = {
        key = "a"
    },
    action = {
        key = "left",
        mods = {"cmd", "shift"}
    }
}, {
    -- キャレット右から行末までを選択
    trigger = {
        key = "f"
    },
    action = {
        key = "right",
        mods = {"cmd", "shift"}
    }
}, {
    -- Cut
    trigger = {
        key = "x"
    },
    action = {
        key = "x",
        mods = {"cmd"}
    }
}, {
    -- Copy
    trigger = {
        key = "c"
    },
    action = {
        key = "c",
        mods = {"cmd"}
    }
}, {
    -- Paste
    trigger = {
        key = "v"
    },
    action = {
        key = "v",
        mods = {"cmd"}
    }
}, {
    -- Close tab
    trigger = {
        key = "w"
    },
    action = {
        key = "w",
        mods = {"cmd"}
    }
}, {
    -- Search tabs
    trigger = {
        key = "i"
    },
    action = {
        key = "i",
        mods = {"alt"}
    }
}, {
    -- 英数入力
    trigger = {
        key = "space"
    },
    action = {
        func = function()
            setIme(false)
        end
    }
}, {
    -- 日本語入力
    trigger = {
        key = "space",
        mods = {"shift"}
    },
    action = {
        func = function()
            setIme(true)
        end
    }
}, {
    -- Undo
    trigger = {
        key = "z"
    },
    action = {
        key = "z",
        mods = {"cmd"}
    }
}, {
    -- Redo
    trigger = {
        key = "z",
        mods = {"shift"}
    },
    action = {
        key = "z",
        mods = {"cmd", "shift"}
    }
}, {
    -- ブラウザバック
    trigger = {
        key = "["
    },
    action = {
        func = function()
            -- マウスのサイドボタン（戻る）をクリック。マウスカーソル下のウィンドウに届く
            hs.eventtap.otherClick(hs.mouse.absolutePosition(), 1000, 3)
        end
    }
}, {
    -- ブラウザフォワード
    trigger = {
        key = "]"
    },
    action = {
        func = function()
            -- マウスのサイドボタン（進む）をクリック。マウスカーソル下のウィンドウに届く
            hs.eventtap.otherClick(hs.mouse.absolutePosition(), 1000, 4)
        end
    }
}}

local function matchModifiers(eventFlags, triggerMods)
    for _, modKey in ipairs(MODIFIER_KEYS) do
        local hasFlag = eventFlags[modKey] or false
        local needsFlag = triggerMods and contains(triggerMods, modKey) or false
        if hasFlag ~= needsFlag then
            return false
        end
    end
    return true
end

F18Pressed = false
-- F18 押下中に握りつぶしたキー（対応する keyUp も握りつぶす）
F18SwallowedKeys = {}

F18Tap = hs.eventtap.new({hs.eventtap.event.types.keyDown, hs.eventtap.event.types.keyUp}, function(event)
    -- hs.eventtap.keyStroke で自分が送出したイベントはそのまま通す
    if event:getProperty(hs.eventtap.event.properties.eventSourceUnixProcessID) == hs.processInfo.processID then
        return false
    end

    local keyCode = event:getKeyCode()
    local char = hs.keycodes.map[keyCode]
    local isKeyDown = event:getType() == hs.eventtap.event.types.keyDown

    if char == "f18" then
        F18Pressed = isKeyDown
        return true
    end

    if not isKeyDown then
        if F18SwallowedKeys[keyCode] then
            F18SwallowedKeys[keyCode] = nil
            return true
        end
        return false
    end

    onTyping()

    if not F18Pressed then
        return false
    end

    local flags = event:getFlags()
    for _, mapping in ipairs(f18Keymap) do
        if mapping.trigger.key == char and matchModifiers(flags, mapping.trigger.mods) then
            if mapping.action.func then
                mapping.action.func()
            else
                hs.eventtap.keyStroke(mapping.action.mods or {}, mapping.action.key, 0)
            end
            break
        end
    end

    -- 未定義のキーも含め、F18 押下中のキー入力はすべて握りつぶす
    F18SwallowedKeys[keyCode] = true
    return true
end)

F18Tap:start()

hs.keycodes.inputSourceChanged(onInputSourceChanged)
onInputSourceChanged()

log("F18キーリマップが有効になりました")
log("使用可能なキーマップ:")
for _, mapping in ipairs(f18Keymap) do
    local triggerMods = mapping.trigger.mods or {}
    local triggerModStr = #triggerMods > 0 and table.concat(triggerMods, "+") .. "+" or ""

    if mapping.action.func then
        log("  F18 + " .. triggerModStr .. mapping.trigger.key .. " → [function]")
    else
        local actionMods = mapping.action.mods or {}
        local actionModStr = #actionMods > 0 and table.concat(actionMods, "+") .. "+" or ""
        log("  F18 + " .. triggerModStr .. mapping.trigger.key .. " → " .. actionModStr .. mapping.action.key)
    end
end
