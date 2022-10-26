tell the application "Finder"
	-- Change this if your drive is not called "Macintosh HD"
	set drive_name to "Macintosh HD"
	
	set free_bytes to (free space of disk drive_name)
	set free_Gbytes to (free_bytes / (1024 * 1024 * 0.1024) div 100) / 100
	return drive_name & " has " & free_Gbytes & " GB free"
end tell