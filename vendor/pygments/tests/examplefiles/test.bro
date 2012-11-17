@load notice
@load utils/thresholds

module SSH;

export {
	redef enum Log::ID += { SSH };

	redef enum Notice::Type += {
		Login,
		Password_Guessing,
		Login_By_Password_Guesser,
		Login_From_Interesting_Hostname,
		Bytecount_Inconsistency,
	};

	type Info: record {
		ts:              time         &log;
		uid:             string       &log;
		id:              conn_id      &log;
		status:          string       &log &optional;
		direction:       string       &log &optional;
		remote_location: geo_location &log &optional;
		client:          string       &log &optional;
		server:          string       &log &optional;
		resp_size:       count        &log &default=0;
		
		## Indicate if the SSH session is done being watched.
		done:            bool         &default=F;
	};

	const password_guesses_limit = 30 &redef;
	
	# The size in bytes at which the SSH connection is presumed to be
	# successful.
	const authentication_data_size = 5500 &redef;
	
	# The amount of time to remember presumed non-successful logins to build
	# model of a password guesser.
	const guessing_timeout = 30 mins &redef;

	# The set of countries for which you'd like to throw notices upon successful login
	#   requires Bro compiled with libGeoIP support
	const watched_countries: set[string] = {"RO"} &redef;

	# Strange/bad host names to originate successful SSH logins
	const interesting_hostnames =
			/^d?ns[0-9]*\./ |
			/^smtp[0-9]*\./ |
			/^mail[0-9]*\./ |
			/^pop[0-9]*\./  |
			/^imap[0-9]*\./ |
			/^www[0-9]*\./  |
			/^ftp[0-9]*\./  &redef;

	# This is a table with orig subnet as the key, and subnet as the value.
	const ignore_guessers: table[subnet] of subnet &redef;
	
	# If true, we tell the event engine to not look at further data
	# packets after the initial SSH handshake. Helps with performance
	# (especially with large file transfers) but precludes some
	# kinds of analyses (e.g., tracking connection size).
	const skip_processing_after_detection = F &redef;
	
	# Keeps count of how many rejections a host has had
	global password_rejections: table[addr] of TrackCount 
		&write_expire=guessing_timeout
		&synchronized;

	# Keeps track of hosts identified as guessing passwords
	# TODO: guessing_timeout doesn't work correctly here.  If a user redefs
	#       the variable, it won't take effect.
	global password_guessers: set[addr] &read_expire=guessing_timeout+1hr &synchronized;
	
	global log_ssh: event(rec: Info);
}

# Configure DPD and the packet filter
redef capture_filters += { ["ssh"] = "tcp port 22" };
redef dpd_config += { [ANALYZER_SSH] = [$ports = set(22/tcp)] };

redef record connection += {
	ssh: Info &optional;
};

event bro_init()
{
	Log::create_stream(SSH, [$columns=Info, $ev=log_ssh]);
}

function set_session(c: connection)
	{
	if ( ! c?$ssh )
		{
		local info: Info;
		info$ts=network_time();
		info$uid=c$uid;
		info$id=c$id;
		c$ssh = info;
		}
	}

function check_ssh_connection(c: connection, done: bool)
	{
	# If done watching this connection, just return.
	if ( c$ssh$done )
		return;
	
	# If this is still a live connection and the byte count has not
	# crossed the threshold, just return and let the resheduled check happen later.
	if ( !done && c$resp$size < authentication_data_size )
		return;

	# Make sure the server has sent back more than 50 bytes to filter out
	# hosts that are just port scanning.  Nothing is ever logged if the server
	# doesn't send back at least 50 bytes.
	if ( c$resp$size < 50 )
		return;

	local status = "failure";
	local direction = Site::is_local_addr(c$id$orig_h) ? "to" : "from";
	local location: geo_location;
	location = (direction == "to") ? lookup_location(c$id$resp_h) : lookup_location(c$id$orig_h);
	
	if ( done && c$resp$size < authentication_data_size )
		{
		# presumed failure
		if ( c$id$orig_h !in password_rejections )
			password_rejections[c$id$orig_h] = new_track_count();
			
		# Track the number of rejections
		if ( !(c$id$orig_h in ignore_guessers &&
		       c$id$resp_h in ignore_guessers[c$id$orig_h]) )
			++password_rejections[c$id$orig_h]$n;
			
		if ( default_check_threshold(password_rejections[c$id$orig_h]) )
			{
			add password_guessers[c$id$orig_h];
			NOTICE([$note=Password_Guessing,
			        $conn=c,
			        $msg=fmt("SSH password guessing by %s", c$id$orig_h),
			        $sub=fmt("%d failed logins", password_rejections[c$id$orig_h]$n),
			        $n=password_rejections[c$id$orig_h]$n]);
			}
		} 
	# TODO: This is to work around a quasi-bug in Bro which occasionally 
	#       causes the byte count to be oversized.
	#   Watch for Gregors work that adds an actual counter of bytes transferred.
	else if ( c$resp$size < 20000000 ) 
		{ 
		# presumed successful login
		status = "success";
		c$ssh$done = T;

		if ( c$id$orig_h in password_rejections &&
		     password_rejections[c$id$orig_h]$n > password_guesses_limit &&
		     c$id$orig_h !in password_guessers )
			{
			add password_guessers[c$id$orig_h];
			NOTICE([$note=Login_By_Password_Guesser,
			        $conn=c,
			        $n=password_rejections[c$id$orig_h]$n,
			        $msg=fmt("Successful SSH login by password guesser %s", c$id$orig_h),
			        $sub=fmt("%d failed logins", password_rejections[c$id$orig_h]$n)]);
			}
		
		local message = fmt("SSH login %s %s \"%s\" \"%s\" %f %f %s (triggered with %d bytes)",
		              direction, location$country_code, location$region, location$city,
		              location$latitude, location$longitude,
		              id_string(c$id), c$resp$size);
		NOTICE([$note=Login,
		        $conn=c,
		        $msg=message,
		        $sub=location$country_code]);
		
		# Check to see if this login came from an interesting hostname
		when ( local hostname = lookup_addr(c$id$orig_h) )
			{
			if ( interesting_hostnames in hostname )
				{
				NOTICE([$note=Login_From_Interesting_Hostname,
				        $conn=c,
				        $msg=fmt("Strange login from %s", hostname),
				        $sub=hostname]);
				}
			}
			
		if ( location$country_code in watched_countries )
			{
			
			}
			
		}
	else if ( c$resp$size >= 200000000 ) 
		{
		NOTICE([$note=Bytecount_Inconsistency,
		        $conn=c,
		        $msg="During byte counting in SSH analysis, an overly large value was seen.",
		        $sub=fmt("%d",c$resp$size)]);
		}

	c$ssh$remote_location = location;
	c$ssh$status = status;
	c$ssh$direction = direction;
	c$ssh$resp_size = c$resp$size;
	
	Log::write(SSH, c$ssh);
	
	# Set the "done" flag to prevent the watching event from rescheduling
	# after detection is done.
	c$ssh$done;
	
	# Stop watching this connection, we don't care about it anymore.
	if ( skip_processing_after_detection )
		{
		skip_further_processing(c$id);
		set_record_packets(c$id, F);
		}
	}

event connection_state_remove(c: connection) &priority=-5
	{
	if ( c?$ssh )
		check_ssh_connection(c, T);
	}

event ssh_watcher(c: connection)
	{
	local id = c$id;
	# don't go any further if this connection is gone already!
	if ( !connection_exists(id) )
		return;

	check_ssh_connection(c, F);
	if ( ! c$ssh$done )
		schedule +15secs { ssh_watcher(c) };
	}

event ssh_server_version(c: connection, version: string) &priority=5
	{
	set_session(c);
	c$ssh$server = version;
	}
	
event ssh_client_version(c: connection, version: string) &priority=5
	{
	set_session(c);
	c$ssh$client = version;
	schedule +15secs { ssh_watcher(c) };
	}
