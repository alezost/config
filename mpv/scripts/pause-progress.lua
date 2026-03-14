function al_progress_on_pause (_name, value)
   if value == true then
      mp.set_property("osd-level", 3)
      mp.command("show-progress")
   else
      mp.set_property("osd-level", 1)
   end
end

mp.observe_property("pause", "bool", al_progress_on_pause)
