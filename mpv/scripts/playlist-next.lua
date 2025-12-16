function al_playlist_next()
   local pl_length = mp.get_property_number("playlist-count")
   local pl_pos    = mp.get_property_number("playlist-pos-1")

   if pl_pos < pl_length then
      mp.command("playlist-next")
   else
      -- Send "client-message" event with "al/playlist-end" argument to
      -- be handled by "emms-mpv.el".
      mp.commandv("script-message", "al/playlist-end")
   end
end

mp.add_key_binding("n", "al_playlist-next", al_playlist_next)
