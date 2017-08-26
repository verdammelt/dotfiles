tell application "System Events"
    if exists (process "Caffeine") then
       tell application "Caffeine"
       if active is false then
	  turn on
       else
	  turn off
       end if
       end tell
    else
       tell application "Caffeine" to activate
    end if
end tell