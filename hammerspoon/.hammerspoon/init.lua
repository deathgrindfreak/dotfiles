-- Show terminal
hs.hotkey.bind({"alt"}, "space", function()
    terminal = hs.application.get("Alacritty")
    if terminal:isFrontmost() then
      terminal:hide()
    else
      terminal:activate()
    end
end)

-- Show emacs
hs.hotkey.bind({"ctrl", "alt"}, "space", function()
    emacs = hs.application.get("Emacs")
    if not emacs:isFrontmost() then
      emacs:activate()
    end
end)

-- Show firefox
hs.hotkey.bind({"ctrl", "cmd"}, "space", function()
    firefox = hs.application.get("Firefox Developer Edition")
    if not firefox:isFrontmost() then
      firefox:activate()
    end
end)

-- lock screen
hs.hotkey.bind({"cmd", "shift"}, "O", function()
  hs.caffeinate.lockScreen()
end)

hs.hotkey.bind({"cmd"}, "home", function()
  hs.spotify.playpause()
end)

hs.hotkey.bind({"cmd"}, "pagedown", function()
  playing = hs.spotify.isPlaying()

  if playing then
    hs.spotify.volumeDown()
  else
    output = hs.audiodevice.defaultOutputDevice()
    output:setVolume(output:volume() - 10)
  end
end)

hs.hotkey.bind({"cmd"}, "pageup", function()
  playing = hs.spotify.isPlaying()

  if playing then
    hs.spotify.volumeUp()
  else
    output = hs.audiodevice.defaultOutputDevice()
    output:setVolume(output:volume() + 10)
  end
end)

caffeine = hs.menubar.new()
function setCaffeineDisplay(state)
    if state then
        caffeine:setTitle("AWAKE")
    else
        caffeine:setTitle("SLEEPY")
    end
end

function caffeineClicked()
    setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"))
end

if caffeine then
    caffeine:setClickCallback(caffeineClicked)
    setCaffeineDisplay(hs.caffeinate.get("displayIdle"))
end
