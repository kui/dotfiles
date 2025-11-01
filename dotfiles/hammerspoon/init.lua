hs.loadSpoon("EmmyLua")

local function log(message)
    hs.console.printStyledtext(os.date("[%Y-%m-%d %H:%M:%S] ") .. message)
end

local MODIFIER_KEYS = {"shift", "cmd", "alt", "ctrl", "fn"}

-- Google日本語入力のSourceID定数
local INPUT_SOURCES = {
    ROMAN = "com.google.inputmethod.Japanese.Roman", -- 英数入力
    JAPANESE = "com.google.inputmethod.Japanese.base" -- 日本語入力
}

local function switchInputSource(targetSourceID, displayName)
    local beforeSourceID = hs.keycodes.currentSourceID()
    hs.keycodes.currentSourceID(targetSourceID)
    local afterSourceID = hs.keycodes.currentSourceID()
    log(displayName .. " - 変更前: " .. beforeSourceID .. " → 変更後: " .. afterSourceID)
    hs.alert.show(displayName)
end

local function contains(array, value)
    for _, item in ipairs(array) do
        if item == value then
            return true
        end
    end
    return false
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
    -- Delete to end of line
    trigger = {
        key = "a"
    },
    action = {
        func = function()
            -- キャレット右側行頭までを削除: Cmd+Shift+Left(行頭まで選択) → Delete
            hs.eventtap.keyStroke({"cmd", "shift"}, "left", 0)
            hs.eventtap.keyStroke({}, "delete", 0)
        end
    }
}, {
    -- Delete to beginning of line
    trigger = {
        key = "f"
    },
    action = {
        func = function()
            -- キャレット左側行末までを削除: Cmd+Shift+Right(行末まで選択) → Delete
            hs.eventtap.keyStroke({"cmd", "shift"}, "right", 0)
            hs.eventtap.keyStroke({}, "delete", 0)
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
            switchInputSource(INPUT_SOURCES.ROMAN, "_A")
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
            switchInputSource(INPUT_SOURCES.JAPANESE, "あ")
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

-- 日本語入力インジケータ
---@type hs.canvas|nil
JapaneseInputIndicator = nil
---@type hs.timer|nil
IndicatorUpdateTimer = nil

-- 日本語入力状態の表示を作成
local function createJapaneseIndicator()
    local indicator = hs.canvas.new({
        x = 0,
        y = 0,
        w = 60,
        h = 60
    })
    ---@cast indicator hs.canvas
    indicator:appendElements({
        type = "rectangle",
        action = "fill",
        fillColor = {
            red = 0.2,
            green = 0.6,
            blue = 1.0,
            alpha = 0.9
        },
        roundedRectRadii = {
            xRadius = 5,
            yRadius = 5
        }
    }, {
        type = "text",
        text = "あ",
        textColor = {
            white = 1.0,
            alpha = 1.0
        },
        textSize = 32,
        textAlignment = "center",
        frame = {
            x = 0,
            y = 11,
            w = 60,
            h = 60
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

    -- マウスカーソルの近くに表示
    local mousePos = hs.mouse.absolutePosition()
    indicator:topLeft({
        x = mousePos.x + 20,
        y = mousePos.y + 20
    })
end

-- 入力ソース変更を監視
local function inputSourceChanged()
    local currentSourceID = hs.keycodes.currentSourceID()

    if currentSourceID == INPUT_SOURCES.JAPANESE then
        -- 日本語入力モード
        if not JapaneseInputIndicator then
            JapaneseInputIndicator = createJapaneseIndicator()
        end
        updateIndicatorPosition(JapaneseInputIndicator)
        JapaneseInputIndicator:show()

        -- マウス追従タイマーを開始（0.05秒ごとに更新）
        if IndicatorUpdateTimer then
            IndicatorUpdateTimer:stop()
        end
        IndicatorUpdateTimer = hs.timer.doEvery(0.05, function()
            updateIndicatorPosition(JapaneseInputIndicator)
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
    end
end

-- 入力ソース変更のウォッチャーを設定
hs.keycodes.inputSourceChanged(inputSourceChanged)

-- 初期状態を設定
inputSourceChanged()

log("日本語入力インジケータが有効になりました")
