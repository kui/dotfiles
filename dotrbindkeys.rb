# -*- coding:utf-8; mode:ruby; -*-

## user settings

# if you use a keyboard which have a left ctrl key at the left of "A" key,
# then you must set false
@swap_left_ctrl_with_caps = true

# for apple keyboard
@swap_left_opt_with_left_cmd = true

# load a local config
@local_config = File.join "#{ENV['HOME']}", "/.rbindkeys.local.rb"
if File.file? @local_config
  puts "load a local config"
  eval File.new(@local_config).read
end

##

if @swap_left_ctrl_with_caps
  pre_bind_key KEY_CAPSLOCK, KEY_LEFTCTRL
  pre_bind_key KEY_LEFTCTRL, KEY_CAPSLOCK
end

if @swap_left_opt_with_left_cmd
  pre_bind_key KEY_LEFTMETA, KEY_LEFTALT
  pre_bind_key KEY_LEFTALT, KEY_LEFTMETA
end

bind_key [KEY_LEFTCTRL, KEY_F], KEY_RIGHT
bind_key [KEY_LEFTCTRL, KEY_B], KEY_LEFT
bind_key [KEY_LEFTCTRL, KEY_P], KEY_UP
bind_key [KEY_LEFTCTRL, KEY_N], KEY_DOWN
bind_key [KEY_LEFTALT, KEY_F], [KEY_LEFTCTRL, KEY_RIGHT]
bind_key [KEY_LEFTALT, KEY_B], [KEY_LEFTCTRL, KEY_LEFT]
bind_key [KEY_LEFTCTRL, KEY_A], KEY_HOME
bind_key [KEY_LEFTCTRL, KEY_E], KEY_END

bind_key [KEY_LEFTCTRL, KEY_V], KEY_PAGEDOWN
bind_key [KEY_LEFTALT, KEY_V], KEY_PAGEUP
bind_key [KEY_LEFTCTRL, KEY_D], KEY_DELETE
bind_key [KEY_LEFTCTRL, KEY_H], KEY_BACKSPACE
bind_key [KEY_LEFTCTRL, KEY_M], KEY_ENTER
bind_key [KEY_LEFTCTRL, KEY_I], KEY_TAB
bind_key [KEY_LEFTCTRL, KEY_LEFTBRACE], KEY_ESC
bind_key [KEY_LEFTCTRL, KEY_S], [KEY_LEFTCTRL, KEY_F]
bind_key [KEY_LEFTCTRL, KEY_SLASH], [KEY_LEFTCTRL, KEY_Z]

# give a block sample
@caps_led_state = 0
bind_key KEY_CAPSLOCK do |event, operator|
  @caps_led_state = @caps_led_state ^ 1
  operator.send_event EV_LED, LED_CAPSL, @caps_led_state
end

# kill line
bind_key [KEY_LEFTCTRL, KEY_K] do |event, operator|
  # select to end of line
  operator.press_key KEY_LEFTSHIFT
  operator.press_key KEY_END
  operator.release_key KEY_END
  operator.release_key KEY_LEFTSHIFT
  operator.send_event EV_SYN, 0, 0 # flush the event buffer

  sleep 0.01

  # cut
  operator.press_key KEY_LEFTCTRL
  operator.press_key KEY_X
  operator.release_key KEY_X
  operator.release_key KEY_LEFTCTRL
end


# region mode
@region_mode = false
def cancel_region op
  op.release_key KEY_LEFTSHIFT
  op.press_key KEY_RIGHT
  op.release_key KEY_RIGHT
  @region_mode = false
end
def start_region op
  operator.press_key KEY_LEFTSHIFT
  @region_mode = true
end
bind_key [KEY_LEFTCTRL, KEY_SPACE] do |event, operator|
  cancel_region operator if @region_mode
  start_region operator
  :ignore
end
bind_key [KEY_LEFTCTRL, KEY_G] do |event, operator|
  if not @region_mode
    :through
  else
    cancel_region operator
    :ignore
  end
end

# binds related kill-ring
bind_key [KEY_LEFTCTRL, KEY_W] do |ev, op|
  op.release_key KEY_LEFTSHIFT if @region_mode

  op.press_key KEY_LEFTCTRL
  op.press_key KEY_X
  op.release_key KEY_X
  op.release_key KEY_LEFTCTRL

  @region_mode = false

  :ignore
end
bind_key [KEY_LEFTALT, KEY_W] do |ev, op|
  op.release_key KEY_LEFTSHIFT if @region_mode
  op.release_key KEY_LEFTALT

  op.press_key KEY_LEFTCTRL
  op.press_key KEY_C
  op.release_key KEY_C
  op.release_key KEY_LEFTCTRL

  # cancel the selection
  if @region_mode
    op.press_key KEY_RIGHT
    op.release_key KEY_RIGHT
  end

  op.press_key KEY_LEFTALT

  @region_mode = false

  :ignore
end
bind_key [KEY_LEFTCTRL, KEY_Y], [KEY_LEFTCTRL,KEY_V]

bind_key [KEY_LEFTALT, KEY_R], [KEY_LEFTCTRL,KEY_C]

# 2 stroke binds
bind_prefix_key [KEY_LEFTCTRL, KEY_X] do
  bind_key KEY_K, [KEY_LEFTCTRL, KEY_W]
  bind_key KEY_S, [KEY_LEFTCTRL, KEY_S]
  bind_key KEY_B, [KEY_LEFTCTRL, KEY_TAB]
  bind_key KEY_H, [KEY_LEFTCTRL, KEY_A]
  bind_key [KEY_LEFTCTRL, KEY_G], :ignore
  bind_key [KEY_LEFTCTRL, KEY_C], [KEY_LEFTALT, KEY_F4]
end

# settings per window class (or title)

# through all key inputs if active
window(:through, :class => /^(?:gnome-terminal|roxterm|sakura|clusterssh|xterm)$/)

# add new bind_key to default binds
window(@default_bind_resolver, :class => /google-chrome/) do
  # search
  bind_key [KEY_LEFTCTRL, KEY_S], [KEY_LEFTCTRL, KEY_F]
end

# add new bind_key to default binds
window(@default_bind_resolver, :class => /Eclipse/) do
  bind_key [KEY_LEFTCTRL, KEY_TAB], [KEY_LEFTSHIFT, KEY_LEFTCTRL, KEY_E]

  # kill line
  bind_key [KEY_LEFTCTRL, KEY_K] do |event, operator|
    # select to end of line
    operator.press_key KEY_LEFTSHIFT
    operator.press_key KEY_END
    operator.release_key KEY_END
    operator.release_key KEY_LEFTSHIFT
    operator.send_event EV_SYN, 0, 0 # flush the event buffer

    sleep 0.05

    # cut
    operator.press_key KEY_LEFTCTRL
    operator.press_key KEY_X
    operator.release_key KEY_X
    operator.release_key KEY_LEFTCTRL
  end
end
