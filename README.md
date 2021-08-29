This program is a bit of a mess right now, but works.

Google login does not like automated logging in to their system.  As a result,
we must use Firefox, log in to the desired site using the default profile, and then
use that default path in the application (it is a parameter now).  The default location on Mac is `/Users/<<username>>/Library/Application Support/Firefox/Profiles/*.default-release-*`