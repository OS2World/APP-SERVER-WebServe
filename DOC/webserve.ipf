:userdoc.
:title.WebServe documentation
:docprof toc=123.

.***********************************
.*   INTRODUCTION
.***********************************

:h1.Introduction

:p.
WebServe is an HTTP web server. It is designed as a lightweight
server, which means that it doesn't attempt to include a huge
range of features. It should, however, work with most personal
web sites, and possibly some business ones.
:p.
It should give fast response with a low load on the processor,
but no comparative timing tests have been done.
:p.
It is distributed as open-source freeware subject to the GNU GPL
licence. The source code is included in the zip file.

:p.This documentation is for version 1.6.

:p.
:hp2.Disclaimer of Warranty:ehp2.

:sl compact.
:li.
:hp1.
This Product is provided "as-is", without warranty of any
kind, either expressed or implied, including, but not limited to,
the implied warranties of merchantability and fitness for a
particular purpose. The entire risk as to the quality and
performance of the Product is with you. Should the Product prove
defective, the full cost of repair, servicing, or correction lies
with you.
:ehp1.
:esl.

:p.
The author of WebServe is Peter Moylan, peter@pmoylan.org.

:p.
The latest version of WebServe is normally kept at
http&colon.&slash.&slash.www.pmoylan.org/pages/os2/software.html.
Information about other software on this site may also be found on that page.

:p.
:hp2.Getting information about new versions:ehp2.

:p.
You can, if you wish, join a mailing list for announcements about
new releases of my software. The mailing list webserve-list@os2voice.org is a
general forum for discussions and questions about WebServe. To join
this list, send an
e-mail to majormajor@os2voice.org. The subject line is not
important and may be anything. In the body of the message, put the
lines
:xmp.

       subscribe webserve-list
       end
:exmp.

:p.To have yourself removed from a list, send a similar e-mail but
using the command "unsubscribe" instead of "subscribe".

:p.
:hp2.Finding out which version you have:ehp2.

:p.If you have lost track of which version of WebServe you have, open
an OS/2 command window and type the command
:xmp.

       bldlevel webserve.exe
:exmp.

:p.The "bldlevel" command is an OS/2 feature, not a WebServe feature.

.***********************************
.*   REGISTRATION AND PREREQUISITES
.***********************************

:h1 id=register.Registration and prerequisites

:hp2.Registration:ehp2.

:p.This software is freeware, and does not require registration.

.***********************************
.*   PREREQUISITES
.***********************************

:p.:hp2.Prerequisites:ehp2.

:p.:hp3.Getting a domain name:ehp3.

:p.Obviously your server has to be reachable by web clients. If you
want to restrict access to your local network this is easily done, for
example by putting appropriate entries in the HOSTS files of the client
machines. For more typical use, you have to own at least one domain
name.

:p.To buy a domain name, do a web search with terms like
"domain name registration". Make sure to shop around before purchasing,
because prices vary widely. Also check the web for the topic
"domain name scams", because some domain registrars are less than
honest.

:p.You will also need to ensure that a domain name server (DNS)
somewhere provides a mapping from domain name to your IP address.
Some ISPs will do this for you. Some domain registrars will also
offer this for a small extra fee. Otherwise, do a search for DNS
hosting services. Again, be prepared to shop around for a good price.

:p.Domain registrars and DNS providers often also offer web hosting.
You do not need this if you are running your own web server.

:p.:hp3.Ensuring your time zone is set:ehp3.

:p.When you first run WebServe, and assuming that you have enabled
logging to the screen, you should see a log line saying something
like "Time zone is +10&colon.00". If instead that line says
"Time zone is undefined" then your clock software is not storing
the time zone in the OS/2 internal DATETIME structure.

:p.One way to fix this is to get the free TZSet utility from
http&colon.&slash.&slash.www.pmoylan.org&slash.pages&slash.os2&slash.software.html.

.***********************************
.*   LANGUAGE SUPPORT
.***********************************

:h1.Language support

:hp2.Language support:ehp2.

:p.The web server component of WebServe does not need any
language support, because HTTP requests and responses are
governed by a standard that is language-independent. On the
other hand, the Setup program that comes with WebServe can
use any language for which the file setup.xx.lng exists, where
xx represents a language code.

:p.
A language is specified by a code, for example 'en' for English
and 'fr' for French. The usual convention is to use a two-letter
code, although you can go up to 31 letters if you wish.

:p.If you want to add support for a new language, the easiest way is
to copy one of the existing setup.*.lng files and then translate them. The format
should be obvious from looking at the files. If you want to send
those translations to me, I would be pleased to include them in
future distributions of WebServe.

.***********************************
.*   INSTALLATION
.***********************************

:h1 id=Installation.Installation
:hp2.Installation:ehp2.
:p.
See also :link reftype=hd refid=deinstall.De-installation:elink..

:p.You should have received this package in the form of a zip file.
The first step is to unzip the file into a directory of your choice.
(Presumably you have already done this.) You will find that you
have executable files WebServe.exe and Setup.exe, and a few other
files.

:p.Setup.exe sets up the configurable parameters, so you should
run it first. The required settings should be obvious. If
not, you can refer to the :link reftype=hd refid=Configuration.configuration:elink.
section of this manual.

:p.WebServe.exe is the actual server, and you leave
it running all the time. Ultimately you will probably want to
run it minimized or detached, but initially it is a good idea
to run it directly on your desktop, so that you can verify that
it is doing what you expect.

:p.It is a good idea to run DumpINI (see later) after your installation
is up and working, and thereafter at monthly intervals, to ensure that
you have a backup of the configuration.

:p.WebServe.exe has one optional parameter. If you start it with the command
:xmp.

           WEBSERVE -T
:exmp.
then it will take its configurable parameters from the file WEBSERVE.TNI
instead of WEBSERVE.INI. If you start it with the command
:xmp.

           WEBSERVE -I

:exmp.
then it will take its configurable parameters from the WEBSERVE.INI.
 These configuration files are created by the
:link reftype=hd refid=Configuration.Setup:elink. program. In the
present version 'I' is the default if no parameter is specified, but
this might change in a future release.

:p.The character '-' before a parameter is ignored, and therefore
optional, in the current version, but it might become compulsory in a
future version, depending on whether other options are introduced.

:p.If you need to stop WebServe.exe, type CTRL/C. You might need
to type it twice if clients are still connected.
A process killer, e.g. the "Kill" option in TOP, will also work, but
the CTRL/C method is preferred because it makes
WebServe tidy up and exit cleanly.

:p.The server is not going to do anything interesting
until you have run Setup to specify at least one domain. For that
detail, see the
:link reftype=hd refid=configuration.Configuration:elink. section.

.***********************************
.*   DEINSTALLATION
.***********************************

:h1 id=deinstall.De-installation
:hp2.De-installation:ehp2.
:p.
WebServe does not tamper with CONFIG.SYS or with other system files.
If you decide that you don't want to keep it, simply delete
the directory into which you installed it.

:p.You should of course check whether there are any files or
subdirectories that you want to keep. You might, for example, have
put some of your HTML files in a subdirectory, although that would
not be advisable. It is better to have the server in one directory,
and your web site files in an entirely different directory.

.***********************************
.*   CONFIGURATION
.***********************************

:h1 id=configuration.Configuration
:hp2.Configuration:ehp2.

:p.The program called Setup.exe is what you use to configure WebServe.
The configuration parameters are stored in a file called WebServe.INI.
In effect, Setup.exe is an editor for that configuration file.

:p.When you run Setup, you will get a small screen window with Local/Remote
radio buttons and three pushbuttons. The Remote option is for remote
configuration, which is described in a
:link reftype=hd refid=remoteconfig.separate section:elink.. Normally you
should choose the Local option, which will edit the WebServe.INI file that
resides in the same directory as SETUP.EXE. Click on the "GO" button to
start the editing.

:p.If you start Setup with the command
:xmp.            setup -T

:exmp.
then Setup will edit the file WEBSERVE.TNI rather than WEBSERVE.INI. This 'T'
option can, if desired, be combined with the 'L', 'R', and 'G' options
documented below.

:p.If you start Setup with the command
:xmp.            setup -L
:exmp.
then you will bypass the small opening screen and go directly into local editing.

:p.If you start Setup with the command
:xmp.            setup -R

:exmp.
then you will bypass the small opening screen and go directly into remote editing.

:p.If you start Setup with the command
:xmp.            setup -G

:exmp.
then you don't get the small opening screen, and the Local/Remote option
has whatever value it had the last time you ran Setup.

:p.More than one option can be specified, but of course if you try to
specify more than one of 'L', 'R', and 'G' the result is undefined.
These option characters are case-independent, e.g. 'r' means the same as
'R'. In the present version of Setup the '-' character is ignored, so it
does not matter if you omit it. This could change if a future version of Setup
has command-line parameters more than one character long.

:p.After passing the small opening window, you get a notebook with the
following pages.

:ul.
:li.:link reftype=hd refid=Basic.Basic:elink. - some overall program settings.
:li.:link reftype=hd refid=Domains.Domains:elink. - the list of domains handled by the server.
:li.:link reftype=hd refid=Logging.Logging:elink. - options for what sort of logging you want.
:li.:link reftype=hd refid=About.About:elink. - identifies the program version.
:eul.

:p.Depending on the version of Setup you have, you might also have a page about
:ul.
:li.:link reftype=hd refid=oldMIME.MIME:elink. - the MIME types the server will recognise
:eul.
:p.but this is obsolete. The MIME types are now kept in a file called MIME.CFG.

.***********************************
.*   FONTS
.***********************************

:h2 id=configurationfonts.Changing fonts
:hp2.Changing fonts:ehp2.

:p.If you want to change the font used for the Setup notebook pages, drop
a new font from the Font Palette to a page. The change will affect every
page in that notebook. You can use the same method to change the fonts on
the notebook tabs.

:p.Note that the font must be dropped onto the "background" part of the
notebook page. If you drop a new font onto an individual entry field, for
example, the font will change for the current session but it will not
be remembered the next time you open up the notebook. I felt that it was
unlikely that anyone would want a whole lot of different fonts for different
fields.

.***********************************
.*   BASIC
.***********************************

:h2 id=Basic.Basic settings
:hp2.Basic settings:ehp2.

:p.Changing the server port will not have any effect until you restart
WebServe. All other changes, including changes on the other notebook
pages, will take effect as soon as you exit from Setup.

:p.:hp3.Server port:ehp3.

:p.The default port for a web server is port 80. Do not change this
unless you know what you are doing.

:p.:hp3.Timeout:ehp3.

:p.Most clients will disconnect after a couple of minutes of idle time,
but there are also inconsiderate web clients out there. When you
specify a timeout setting, the server will close a client setting
after the specified amount of inactivity. The recommend value is
120 seconds.

:p.:hp3.Max clients:ehp3.

:p.This specifies the maximum number of simultaneous connections, of
course, not the maximum number in total. Once you have this many
clients connected, any new connection attempt will be rejected until
one of the existing clients disconnects.

:p.The setting here will depend on how busy you expect the server to be,
how fast your hardware is, and how many simultaneous threads your
system can support. (Each connection uses up two threads, one for the
connection itself and one for a watchdog timer.) The default value of
100 should be adequate for most situations.

:p.:hp3.Language:ehp3.

:p.This is a code for the language to use in the Setup notebook
itself. If the language code is xy, the notebook labels are taken from
the file called Setup.xy.lng. If this file is missing, the labels will
be in English. (Unless Setup.en.lng is also missing, in which case the
labels might be blank.)

:p.You are free to add your own setup.abc.lng for language "abc". The
easiest way to do this is to make a copy of setup.en.lng, and translate it
in the obvious way.


.***********************************
.*   DOMAINS
.***********************************

:h2 id=Domains.Domains
:hp2.The list of domains handled by this server:ehp2.

:p.The listbox on this page lists the domains that your server will
handle. Initially the list is empty, but it must contain at least one
entry before WebServe can do anything useful.

:p.Use the Add button to specify a new domain. In response to the
"Enter a domain name" prompt, type the domain name followed by
the Enter key. This will bring up a new dialogue, where you can
:link reftype=hd refid=EditDomain.specify the domain properties.:elink.

:p.The other buttons on the right require a list entry to be selected.
You select an entry with a left mouse click.

:p.Use the Delete button to remove a domain that you no longer want
the server to handle, and the Rename button to correct a typing error.

:p.The Promote button is for changing the order of the list entries.
The order does not matter unless you are hosting many domains, but
you might want to control the order for neatness, or a similar reason.

:p.The Edit button lets you
:link reftype=hd refid=EditDomain.modify the domain properties.:elink.
You can get the same result by typing the Enter key when the domain
name is highlighted, or by double-clicking on the domain name.

.***********************************
.*   LOGGING
.***********************************

:h2 id=Logging.Logging
:hp2.Logging options:ehp2.

:p.On this page you can specify either transfer logging, or
transaction logging, or both. The difference is that the transfer log
gives you one entry per client request, while transaction logging
gives a more detailed log.

:p.The "common log" form of transfer log uses a format that is
expected by some web analysis tools. I gather that there is a newer
format, but I am not aware of any OS/2 tools that can handle it. If
there are, let me know and I will look into supporting the newer format.

:p.A transaction log can be sent to any or all of the screen, a disk file,
a pipe, or a syslog daemon.

:p.If you specify logging to disk, you have to specify a file name. For logging
to syslog, you have to specify the syslog host as an IP address. This host
address is normally the loopback address 127.0.0.1, but it could be different
if you are running syslog on a different machine.

:p.If you specify logging to a pipe, the pipe name is \PIPE\WebServeTranslog.
Of course this option is useless unless you have another program running that
will read from the pipe. To write such a program, you can get some ideas from
ftp&colon.&slash.&slash.pmoylan.org/FtpServer/tools/pipelog.zip.
Although this was written for a different application, it should be easy to
adapt.

:p.The "More detailed logging" checkbox at the bottom of this page controls
how much detail is sent to the transaction log. If it is enabled, we log
not only the requests and responses, but also the parameters that go with those
requests and responses.

:p.Logging can produce large log files. It is a good idea to move the log files
to an archive, or even delete them, periodically. To see how to do this, read
the file README.MoveLog in the WebServe distribution.

.***********************************
.*   MIME
.***********************************

:h2 id=oldMIME.MIME
:hp2.The MIME types recognised by the server:ehp2.

:p.If you see this page, you are running an old version of Setup. The MIME types
are no longer configured in Setup. They are instead specified in a file called
:link reftype=hd refid=MIME.MIME.CFG:elink..

.***********************************
.*   ABOUT
.***********************************

:h2 id=About.About
:hp2.About:ehp2.

:p.The function of the "About" notebook page is to identify the author, and to tell you
what version of the program you have.

.***********************************
.*   REMOTE CONFIGURATION
.***********************************

:h1 id=remoteconfig.Remote configuration
:hp2.Remote configuration:ehp2.

:p.Setup also offers the option of remote server administration. That
is, you can run Setup on one computer and use it to configure a copy
of WebServe that is installed on a different computer. To do this, you have
to have the freeware utility INIServe running on the same computer as
WebServe. You can find INIServe at http&colon.&slash.&slash.www.pmoylan.org&slash.pages&slash.os2&slash.

:p.If you select the "Remote" radio button after starting Setup, a "Setup"
pushbutton is enabled. Clicking on this gives you four fields to fill in&colon.

:dl break=all.
:dt.     Hostname
:dd.The name (or IP address) of the machine on which WebServe is running.
:dt.     INIServe port
:dd.The TCP port that INIServe has been configured to listen on. The default
value is 3560.
:dt.     INIServe password
:dd.The password needed to log in to your copy of INIServe.
:dt.     WebServe directory
:dd.The full path name of the directory, on the remote machine, where WebServe
is installed.
:edl.

:p.When you close the Setup window, you can click on the "GO" button to connect
to the remote machine. If this gives a "failed to connect" or similar error
message, it probably means that you don't have INIServe running on the
remote machine, or that you have done something like specifying an incorrect
port number.

:p.Once the connection is made, the operation is the same as for the
case of local configuration.

.***************************************
.*   EDITING THE DOMAIN PROPERTIES
.***************************************

:h1 id=EditDomain.Defining the properties of a domain
:hp2.Defining the properties of a domain:ehp2.

:p.You reach this point by going to the "Domains" page in the Setup
notebook and choosing the "Add" or "Edit" option, or double-clicking with
the mouse on a domain name. This opens up a
new dialogue with the domain properties.

:p.The listbox on this page is to specify alternative names for the domain.
This is needed because, for example, some people might address your web site
as yourdomain.com, or www.yourdomain.com, or perhaps some other possibilities.
If you only want to allow one possibility, then you can leave the "other names"
box empty. More commonly, though, you will want to allow for at least one
alternative.

:p.Names with wildcards are allowed in the listbox. For example, you might
have a domain name yourdomain.com, and then specify *.yourdomain.com as
an alternative name.

:p.To complete specifying the properties of the domain, you must specify
:ul.
:li.:link reftype=hd refid=htmlroot.The HTML root:elink..
:li.:link reftype=hd refid=CGIDir.The CGI directory:elink..
:eul.

:p.Any changes you make will come into effect once you close the Setup program.
It is not necessary to restart WebServe.

:h2 id=htmlroot.The HTML root
:hp2.The HTML root:ehp2.

:p.Every domain must have an HTML root directory. This is where the HTML
files (and associated files such as graphics files) for this domain are kept.
The directory may have subdirectories, depending on how you want to organise
cross-references between HTML files.

:p.The HTML root is normally specified as an absolute path name, i.e. one that
starts either with a drive letter or a '/'. If instead you specify a relative
path, it will be relative to the directory that holds WebServe.exe. Specifying a
relative path is usually not a good idea. It is better to keep the HTML files
separate from the WebServe executable.

:p.When a web client is browsing a domain, it will most probably first try to
fetch the directory "/". Some web servers will return a directory listing in
the case where a directory rather than a file name is specified, but WebServe
considers that to be a security breach. Instead, it will search for files
named "index.shtml", "index.html", "index.htm", in that order, and choose the
first one that is found. The same rule applies for every URL that ends with a
"/", which of course means that you can have index.html files at each level of
your site hierarchy. If none of these files is found, the server will return a
:link reftype=hd refid=404."404 not found":elink.response.

:p.The assumption, of course, is that every resource in a domain can be found,
directly or indirectly, by following links from the index file(s). You are,
of course, free to create "secret" web pages that cannot be found by following
links. Those pages can still be accessed if you tell someone to go directly to
the relevant URL.

:h3 id=robots.robots.txt
:hp2.Robots:ehp2.

:p.It is desirable, but not compulsory, to have a file "robots.txt" in the HTML
root directory of every domain. This is to tell web crawlers - i.e. software that
is trying to index your web site, to collect data for search engines - which
files they may or may not visit. An example robots.txt file might contain the lines

:xmp.
User-agent: msnbot/*
Disallow: /

User-agent: *
Disallow: /cgi-bin/
Disallow: *.zip
:exmp.

:p.The first two lines say that the web crawler called "msnbot" is not allowed to
access anything on this site. The remaining lines say that any other crawler is
denied access to all cgi-bin files and all zip files. (This is more for efficiency than
anything else, although there is also a privacy aspect. There is not much point in
having a search engine index all of your zip files.)

:p.To learn the rules for creating robots.txt files, visit
http&colon.//www.robotstxt.org/


:h3 id=404.404 Not Found
:hp2.404 Not Found:ehp2.

:p.As part of every HTTP transaction, the server returns a three-digit (decimal)
code to indicate success or failure. The most common failure code is 404, which
means "file not found", so it is worth treating this as a special case.

:p.In principle, a "file not found" condition should occur only when you have
faulty links in your web pages. In practice, however, the "not found" condition
is very common because of malicious web crawlers that are trying to find
security holes in your server.

:p.If a file is not found, WebServe will try to find a file whose name is of the
form "/404*.html", where the "*" indicates any arbitrary character string. An
example file "404 NotFound.html" is included in the WebServe distribution; if you
want to use it, copy it into the HTML root directory for each domain. You are of
course free to edit or replace that file. If a file
matching this pattern is not found, WebServe will return a more basic text file.

:h2 id=CGIDir.The CGI directory
:hp2.The CGI directory:ehp2.

:p.WebServe, in common with many other web servers, also supports the concept
of a "cgi-bin" directory. This is to support URLs of the form
:xmp.       http&colon.//domain/cgi-bin/prog?args
:exmp.
where domain is the domain name, "cgi-bin" is a special name to say that we
are asking for a :link reftype=hd refid=CGI.CGI application:elink.,
prog is an executable file, and args are the parameters to be passed to the
executable. This form of URL asks for an executable program or script to
be executed, and the result sent back as a text file to the client.

:p.These CGI applications must be kept in the CGI directory for this domain,
or a subdirectory of the CGI directory. It is possible to write a
CGI application in such a way that it invokes other scripts or executables
elsewhere in the file system, but before doing this you should consider
the possible security implications. The whole point of having a CGI directory
is to have a "sandbox" where CGI applications do not leak out into the
wider file system.

:p.You may specify the CGI directory as an absolute path or a relative path.
If you specify a relative path, it is relative to the HTML root directory
for this domain. The choice here is purely a matter of personal taste.

.***********************************
.*   MIME
.***********************************

:h1 id=MIME.The file MIME.CFG
:hp2.Defining the MIME types recognised by the server:ehp2.

:p.In OS/2 and similar operating systems, the type of a file is usually taken
to mean the filename extension: the part after the last '.'. For example,
in the filename with name "changes.doc.zip", the extension is "zip".
OS/2 also has something called the .TYPE extended attribute, but WebServe does
not yet support this.

:p.In the MIME standard (Multi-Purpose Internet Mail Extensions) the type is
instead what is specified in the "Content-Type:" header line of an e-mail
message. It is usually in the form type/subtype, for example text/plain. The
HTTP protocol also uses headers that are similar to e-mail headers, so the
same "Content-Type:" specification is used.

:p.This means that we need a mapping between filename extensions and MIME types.
That is done in a file called MIME.CFG, which should be in the same directory
as webserve.exe. Each non-comment line in that file has the format
:xmp.       type/subtype EXT
:exmp.
where type/subtype is the MIME type, and EXT is a filename extension. You are
allowed to specify more than one extension for a single MIME type.

:p.A comment line is any line starting with ';'.

:p. The IANA registry at http&colon.//www.iana.org/assignments/media-types/media-types.xhtml
lists a large number of MIME types, but most of them are not likely to be seen on
OS/2 machines. The MIME.CFG that is supplied with WebServe has only a subset of the
official list. You may, if you wish, edit this file to add more types.

:note.If you customise this file, create a backup, because it is likely to be overwritten
if you update to a new release of WebServe.

:note.Avoid trying to associate multiple types with a single extension. For example,
if you have entries
:xmp.
video/mp4 MP4
audio/mp4 MP4
application/mp4 MP4
:exmp.
you have created an ambiguity. WebServe resolves the ambiguity by taking only
the first line and ignoring the others.

.***************************************
.*   CGI
.***************************************

:h1 id=CGI.CGI applications
:hp2.CGI applications:ehp2.

:p.The Common Gateway Interface (CGI) is a feature that many web servers,
including WebServe, implement to allow clients to request the execution of
an executable script on their behalf, and return the results in the form of a web page.
An example of such a request is
:xmp.       http&colon.//www.pmoylan.org/cgi-bin/wft.cmd?D=moylan;P=I004
:exmp.
Here, the /cgi-bin is an indication that this is a CGI request; wft.cmd is the
name of the script; and the part after the '?' gives the parameters that are
to be passed to the script.

:p.Obviously it would be unsafe to allow the execution of any arbitrary script,
so we impose the rule that all such scripts must be in the
:link reftype=hd refid=CGIDir.CGI directory:elink.
for this domain, or a subdirectory of that directory. (The scripts may
call programs outside that directory, but if so you must ask yourself whether
that would create a security hole.) It would still be possible to write
scripts that violate the server's security, but now the security is under the
control of the web site owner, who is presumably the only person able to put
scripts in the CGI directory.

:p.WebServe implements only a subset of CGI as described in
http&colon.//tools.ietf.org/html/rfc3875,
which is a "for information" document rather than a standards-track proposal.
Only the GET form of HTTP request is supported, and most of the options are
not implemented. We require that the script produce an HTML document with header
lines, a blank line, and then the HTML body. If there are no header lines,
the output must begin with a blank line. The requirement for header lines is
relaxed in the case of an
:link reftype=hd refid=SSI.SSI #exec directive:elink.,
which is covered in a separate section.

:p.The script itself may be anything that can be invoked from an OS/2 command
shell: a Rexx script, a Perl script, a *.exe executable program, and a few
other less common possibilities.

:p.The information passed to the script is passed in the form of OS/2 environment
variables. At present WebServe passes only two environment variables:
:dl compact tsize=40.
:dt.    QUERY_STRING
:dd.the part after the '?' in the request.
:dt.    HTTP_ACCEPT_LANGUAGE
:dd.a string that specifies the caller's preferred language(s)
:edl.
:p.but more variables can be added on request.

.***************************************
.*   SSI
.***************************************

:h1 id=SSI.Server-side include
:hp2.Server-side include:ehp2.

:p.Server-side Include (SSI) is a simple facility for allowing HTML documents
to include other documents. An SSI directive has the form of an HTML comment,
for example
:xmp.    <!--#include virtual="header.txt" -->
:exmp.
This is to ensure that any directive that is not understood by the server
is ignored.

:p.For security reasons, we do not allow ".." to occur anywhere in the
filename part of an SSI directive.

:p.You should be aware that SSI has severe performance implications. The
obvious problem is that any document where SSI is enabled must be scanned,
to see whether it contains any SSI directives, before sending it to the
client. This, however, is not the major overhead. If you look at your web log,
you will see that the majority of HTTP requests produce a response
"304 Not Modified", where the document is not returned to the client.
That is because most clients check the timestamp on the document when
requesting it, so that no transfer is needed if the document has not changed.
If the document has SSI inclusions, then we can no longer trust the
"last modified" timestamp for the document.

:p.For this reason, WebServe does SSI processing only for documents whose
name has the form "*.shtml". Ordinary HTML documents are not processed.

:p.As implemented in WebServe, unlimited nesting is allowed. That is, an
included document, or the output of an #exec script, can specify more
inclusions. There is no check for infinite nesting, because that will become
obvious the first time an HTML author tests his/her document, at which time
the error can be corrected.

:p.There is very little error checking in the WebServe implementation of SSI.
This is again because any errors are easily detected and corrected when a
web page is tested.

:p.WebServe allows two kinds of SSI directive.
:dl compact.
:dt.:link reftype=hd refid=include.#include:elink.
:dd.including another document
:dt.:link reftype=hd refid=exec.#exec:elink.
:dd.executing a script
:edl.

:h2 id=include. #include
:hp2.The #include directive:ehp2.

:p.The #include directive has two variants, as shown in the following examples.
:xmp.    <!--#include virtual="menu.txt" -->
    <!--#include file="footer.html" -->
:exmp.
The Apache specification requires a space character before the "-->" comment
terminator, but WebServe does not enforce this.

:p.The "virtual" version is the most common. The file specification is relative to
the HTML root directory for this domain. (The Apache specification also allows a
CGI script to be specified in this case, but we do not allow that, on the grounds
that it duplicates the function of a #exec directive.) The "file" variant specifies
a filename relative to the current directory.

:p.Nesting is permitted. That is, the included file may contain other SSI directives.

:h2 id=exec.#exec
:hp2.The #exec directive:ehp2.

:p.The #exec directive also has two variants, as shown in these examples.
:xmp.    <!--#exec cgi="/cgi-bin/foo.cgi" -->
    <!--#exec cmd="format c:" -->
:exmp.
The second version, which allows any Unix shell command, is currently disabled,
because I cannot work out how to
make it safe. The first version requests the execution of a
:link reftype=hd refid=CGI.CGI application:elink.,
with the result included into the file being processed. It is possible,
although unlikely, that that application will generate more SSI directives.

:p.REMARK. Normally, a CGI application will produce some header lines
followed by an HTML file. In the present case, however, we expect the CGI
application to produce some text suitable for inclusion in the current
document, so that it is more likely that the CGI application will
produce some "plain text" output with no header lines.

.***************************************
.*   DUMPINI AND LOADINI
.***************************************

:h1 id=DumpINILoadINI.The DumpINI and LoadINI utilities

:p.Most of the configuration data for WebServe, including details such
as the domain directories, are kept either in a
binary file called WEBSERVE.INI, or a "plain text" file called WEBSERVE.TNI.
(See the pages about
:link reftype=hd refid=Installation.installation:elink.
and
:link reftype=hd refid=Configuration.configuration:elink.
to see which of these will be used.)
The two programs described on this page
allow you to convert data between these two formats.

:p.These two utilities are not included in the WebServe distribution.
They are instead part of the freeware GenINI package, which can be fetched from
http&colon.&slash.&slash.www.pmoylan.org/pages/os2/software.html.

:p.:hp2.DumpINI:ehp2.

:p.The command
:xmp.      dumpini webserve
:exmp.
processes WEBSERVE.INI and creates a new file called
WEBSERVE.TNI. This new file contains the same data, but in human-readable
form.

:p.:hp2.LoadINI:ehp2.

:p.The command
:xmp.      loadini webserve
:exmp.
reads WEBSERVE.TNI and loads the information back
into the main INI file called WEBSERVE.INI.

:h1.Why did I call it WebServe?

:hp2.Why did I call it WebServe?:ehp2.

:p.All the good names I could think of were already taken.

:euserdoc.

