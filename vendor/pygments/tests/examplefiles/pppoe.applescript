tell application "System Events"
    tell network preferences
        tell current location
            set aPPPoEService to a reference to (first service whose kind is 10)
            if exists aPPPoEService then
                connect aPPPoEService
            end if
        end tell
    end tell
end tell
