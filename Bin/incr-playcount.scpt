#!/usr/bin/osascript

tell application "iTunes"
	if selection is not {} then -- there ARE tracks selected...
		set mySelection to selection
		repeat with aTrack in mySelection
                       set trackName to (get name of aTrack)
                       set trackPlayed to (get played count of aTrack) + 1
                       set played count of aTrack to trackPlayed
		end repeat
	end if
end tell
