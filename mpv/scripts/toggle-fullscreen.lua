fs_state = mp.get_property("fullscreen")

function al_fs_on_pause (_name, value)
    if value == true then
        fs_state = mp.get_property("fullscreen")
        mp.set_property("fullscreen", "no")
        -- For some reason, it shows progress only when we pause from a
        -- fullscreen (maybe an mpv bug).
        mp.command("show-progress")
    else
        mp.set_property("fullscreen", fs_state)
    end
end

mp.observe_property("pause", "bool", al_fs_on_pause)
