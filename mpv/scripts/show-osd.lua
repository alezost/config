function al_show_osd (_event)
    mp.command("show-progress")
end

function al_osd_on_pause (_name, _value)
    al_show_osd()
end

mp.register_event("file-loaded", al_show_osd)
mp.register_event("seek", al_show_osd)
mp.observe_property("pause", "bool", al_osd_on_pause)
