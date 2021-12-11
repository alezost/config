function al_show_osd (_event)
    mp.command("show-progress")
end

mp.register_event("file-loaded", al_show_osd)
