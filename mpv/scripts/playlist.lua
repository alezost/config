function al_playlist_prev()
   local pl_length = mp.get_property_number("playlist-count")

   if 1 < pl_length then
      mp.command("playlist-prev")
   else
      -- Send "client-message" event to be handled by "emms-mpv.el".
      mp.commandv("script-message", "al/playlist-prev")
   end
end

function al_playlist_next()
   local pl_length = mp.get_property_number("playlist-count")
   local pl_pos    = mp.get_property_number("playlist-pos-1")

   if pl_pos < pl_length then
      mp.command("playlist-next")
   else
      mp.commandv("script-message", "al/playlist-next")
   end
end

mp.add_key_binding("h", "al_playlist-prev", al_playlist_prev)
mp.add_key_binding("n", "al_playlist-next", al_playlist_next)
