function al_show_osd (_event)
    mp.commandv("show-text",
                mp.get_property_osd("duration")
                .. " (" .. mp.get_property_osd("file-size") .. ")\n" ..
                mp.get_property("filename/no-ext"),
                4000)
end

mp.register_event("file-loaded", al_show_osd)
mp.add_key_binding("b", al_show_osd)
