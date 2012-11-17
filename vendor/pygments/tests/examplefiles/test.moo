you_lose_msg = "Either that person does not exist, or has a different password.";
if (!(caller in {#0, this}))
  return E_PERM;
  "...caller isn't :do_login_command...";
elseif (args && (args[1] == "test"))
  return this:test(@listdelete(args, 1));
elseif (!(length(args) in {1, 2}))
  notify(player, tostr("Usage:  ", verb, " <existing-player-name> <password>"));
elseif (!valid(candidate = this:_match_player(name = strsub(args[1], " ", "_"))))
  if (name == "guest")
    "must be no guests";
    this:notify_lines(this:registration_text("guest"));
  else
    notify(player, you_lose_msg);
  endif
  "...unknown player...";
elseif (is_clear_property(candidate, "password") || ((typeof(candidate.password) == STR) && ((length(candidate.password) < 2) || (crypt({@args, ""}[2], candidate.password) != candidate.password))))
  notify(player, you_lose_msg);
  "...bad password...";
  server_log(tostr("FAILED CONNECT: ", args[1], " (", candidate, ") on ", connection_name(player), ($string_utils:connection_hostname(connection_name(player)) in candidate.all_connect_places) ? "" | "******"));
elseif (((candidate.name == "guest") && this.sitematch_guests) && valid(foreigner = $country_db:get_guest()))
  notify(player, tostr("Okay,...  Logging you in as `", foreigner:name(), "'"));
  this:record_connection(foreigner);
  return foreigner;
elseif ((parent(candidate) == $guest) && (!valid(candidate = candidate:defer())))
  if (candidate == #-3)
    notify(player, "Sorry, guest characters are not allowed from your site right now.");
  elseif (candidate == #-2)
    this:notify_lines(this:registration_text("blacklisted", "Sorry, guest characters are not allowed from your site."));
  elseif (candidate == #-4)
    this:notify_lines(this:registration_text("guest"));
  else
    notify(player, "Sorry, all of our guest characters are in use right now.");
  endif
else
  if ((!(name in candidate.aliases)) && (name != tostr(candidate)))
    notify(player, tostr("Okay,... ", name, " is in use.  Logging you in as `", candidate:name(), "'"));
  endif
  if (this:is_newted(candidate))
    notify(player, "");
    notify(player, this:newt_message_for(candidate));
    notify(player, "");
  else
    this:record_connection(candidate);
    if (verb[1] == "s")
      candidate.use_do_command = 0;
    endif
    return candidate;
  endif
endif
return 0;