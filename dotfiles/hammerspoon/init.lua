hs.loadSpoon("EmmyLua")

local function log(message)
    hs.console.printStyledtext(os.date("[%Y-%m-%d %H:%M:%S] ") .. message)
end

local MODIFIER_KEYS = {"shift", "cmd", "alt", "ctrl", "fn"}

-- 想定している入力ソース
-- それぞれ先にあるものが優先される
local INPUT_SOURCES = {
    -- 英数入力モード
    ROMAN = {"com.google.inputmethod.Japanese.Roman", "com.apple.inputmethod.Kotoeri.RomajiTyping.Roman"},
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

local function switchInputSource(targetSources)
    for _, sourceID in ipairs(targetSources) do
        if hs.keycodes.currentSourceID(sourceID) then
            return
        end
    end
end

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
    -- Select to end of line
    trigger = {
        key = "a"
    },
    action = {
        func = function()
            -- キャレット右側行頭までを選択
            hs.eventtap.keyStroke({"cmd", "shift"}, "left", 0)
        end
    }
}, {
    -- Select to beginning of line
    trigger = {
        key = "f"
    },
    action = {
        func = function()
            -- キャレット左側行末までを選択
            hs.eventtap.keyStroke({"cmd", "shift"}, "right", 0)
        end
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
    -- Switch to Roman input
    trigger = {
        key = "space"
    },
    action = {
        func = function()
            switchInputSource(INPUT_SOURCES.ROMAN)
        end
    }
}, {
    -- Switch to Japanese input
    trigger = {
        key = "space",
        mods = {"shift"}
    },
    action = {
        func = function()
            switchInputSource(INPUT_SOURCES.JAPANESE)
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

F18Tap = hs.eventtap.new({hs.eventtap.event.types.keyDown, hs.eventtap.event.types.keyUp}, function(event)
    local keyCode = event:getKeyCode()
    local char = hs.keycodes.map[keyCode]
    local eventType = event:getType()

    if char == "f18" then
        F18Pressed = eventType == hs.eventtap.event.types.keyDown
        return true
    end

    if F18Pressed and eventType == hs.eventtap.event.types.keyDown then
        local flags = event:getFlags()
        for _, mapping in ipairs(f18Keymap) do
            if mapping.trigger.key == char and matchModifiers(flags, mapping.trigger.mods) then
                if mapping.action.func then
                    mapping.action.func()
                else
                    local actionMods = mapping.action.mods or {}
                    hs.eventtap.keyStroke(actionMods, mapping.action.key, 0)
                end
                return true
            end
        end
    end

    return false
end)

F18Tap:start()

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

-- ========================================
-- 日本語入力インジケータ
-- ========================================

-- インジケータの設定定数
local INDICATOR_CONFIG = {
    SIZE = {
        WIDTH = 60,
        HEIGHT = 60
    },
    TEXT = {
        CONTENT = "あ",
        SIZE = 32,
        Y_OFFSET = 11 -- テキストの垂直位置調整
    },
    COLOR = {
        BACKGROUND = {
            red = 0.2,
            green = 0.6,
            blue = 1.0,
            alpha = 0.9
        },
        TEXT = {
            white = 1.0,
            alpha = 1.0
        }
    },
    RADIUS = {
        X = 5,
        Y = 5
    },
    POSITION = {
        OFFSET = 20 -- マウスカーソルからのオフセット
    },
    TIMER = {
        UPDATE_INTERVAL = 0.05 -- マウス追従の更新間隔（秒）
    }
}

-- 日本語入力状態の表示を作成
local function createJapaneseIndicator()
    local indicator = hs.canvas.new({
        x = 0,
        y = 0,
        w = INDICATOR_CONFIG.SIZE.WIDTH,
        h = INDICATOR_CONFIG.SIZE.HEIGHT
    })
    ---@cast indicator hs.canvas
    indicator:appendElements({
        type = "rectangle",
        action = "fill",
        fillColor = INDICATOR_CONFIG.COLOR.BACKGROUND,
        roundedRectRadii = {
            xRadius = INDICATOR_CONFIG.RADIUS.X,
            yRadius = INDICATOR_CONFIG.RADIUS.Y
        }
    }, {
        type = "text",
        text = INDICATOR_CONFIG.TEXT.CONTENT,
        textColor = INDICATOR_CONFIG.COLOR.TEXT,
        textSize = INDICATOR_CONFIG.TEXT.SIZE,
        textAlignment = "center",
        frame = {
            x = 0,
            y = INDICATOR_CONFIG.TEXT.Y_OFFSET,
            w = INDICATOR_CONFIG.SIZE.WIDTH,
            h = INDICATOR_CONFIG.SIZE.HEIGHT
        }
    })

    -- レベルを設定（他のウィンドウの上に表示）
    indicator:level("floating")
    indicator:behavior("canJoinAllSpaces")

    return indicator
end

-- インジケータの位置をマウスカーソルに追従させる
local function updateIndicatorPosition(indicator)
    if not indicator then
        return
    end

    -- マウスカーソルの位置を取得
    local mousePos = hs.mouse.absolutePosition()

    -- 画面のサイズを取得
    local screen = hs.mouse.getCurrentScreen()
    if not screen then
        return
    end
    local screenFrame = screen:frame()

    -- デフォルトは右下
    local x = mousePos.x + INDICATOR_CONFIG.POSITION.OFFSET
    local y = mousePos.y + INDICATOR_CONFIG.POSITION.OFFSET

    -- 画面右端に近い場合は左側に表示
    if mousePos.x + INDICATOR_CONFIG.POSITION.OFFSET + INDICATOR_CONFIG.SIZE.WIDTH > screenFrame.x + screenFrame.w then
        x = mousePos.x - INDICATOR_CONFIG.POSITION.OFFSET - INDICATOR_CONFIG.SIZE.WIDTH
    end

    -- 画面下端に近い場合は上側に表示
    if mousePos.y + INDICATOR_CONFIG.POSITION.OFFSET + INDICATOR_CONFIG.SIZE.HEIGHT > screenFrame.y + screenFrame.h then
        y = mousePos.y - INDICATOR_CONFIG.POSITION.OFFSET - INDICATOR_CONFIG.SIZE.HEIGHT
    end

    indicator:topLeft({
        x = x,
        y = y
    })
end

---@type hs.canvas|nil
JapaneseInputIndicator = nil
---@type hs.timer|nil
IndicatorUpdateTimer = nil

-- ========================================
-- マウス移動による自動入力切り替え
-- ========================================

-- マウス移動の設定
local MOUSE_CONFIG = {
    MOVEMENT_THRESHOLD = 300
}

---@type hs.geometry|nil
LastMousePosition = nil

-- 2点間の距離を計算
local function calculateDistance(point1, point2)
    local dx = point2.x - point1.x
    local dy = point2.y - point1.y
    return math.sqrt(dx * dx + dy * dy)
end

-- マウス移動を監視して、閾値を超えたら英数入力に切り替え
local function checkMouseMovement()
    local currentPos = hs.mouse.absolutePosition()

    if LastMousePosition then
        local distance = calculateDistance(LastMousePosition, currentPos)

        -- 閾値を超えた場合、英数入力に切り替え
        if distance >= MOUSE_CONFIG.MOVEMENT_THRESHOLD then
            switchInputSource(INPUT_SOURCES.ROMAN)
        end
    end

    LastMousePosition = currentPos
end

-- 入力ソース変更を監視
local function inputSourceChanged()
    local currentSourceID = hs.keycodes.currentSourceID()

    if contains(INPUT_SOURCES.JAPANESE, currentSourceID) then
        -- 日本語入力モード
        if not JapaneseInputIndicator then
            JapaneseInputIndicator = createJapaneseIndicator()
        end
        updateIndicatorPosition(JapaneseInputIndicator)
        JapaneseInputIndicator:show()

        -- タイマーを開始（インジケータ更新とマウス移動監視）
        if IndicatorUpdateTimer then
            IndicatorUpdateTimer:stop()
        end
        LastMousePosition = hs.mouse.absolutePosition()
        IndicatorUpdateTimer = hs.timer.doEvery(INDICATOR_CONFIG.TIMER.UPDATE_INTERVAL, function()
            updateIndicatorPosition(JapaneseInputIndicator)
            checkMouseMovement()
        end)
    else
        -- 英数入力モード
        if JapaneseInputIndicator then
            JapaneseInputIndicator:hide()
        end

        -- タイマーを停止
        if IndicatorUpdateTimer then
            IndicatorUpdateTimer:stop()
            IndicatorUpdateTimer = nil
        end
        LastMousePosition = nil
    end
end

hs.keycodes.inputSourceChanged(inputSourceChanged)
inputSourceChanged()
