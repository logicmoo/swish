# SWISH: A web based SWI-Prolog environment

## integrated with content and examples from:
   
   The Cplint suite
   Trill-on-Swish
   Logtalk
   PFC/LogicMOO/PrologMUD
   ClioPatria Integration


## Online version

SWISH can be used to access [SWI-Prolog](http://www.swi-prolog.org) at
the address below. We try to keep this server continuously online. You
can use this server for playing, courses or sharing and discussing
ideas. We have not yet dealt with scalable hosting nor with really
reliable and scalable storage for saved programs. We hope to keep all
your programs online for at least multiple years.

  - http://swish.swi-prolog.org/

## Installation

### Get submodules

`cd` to your swish root directory and

    git submodule update --init

If you have `make` installed you  can   configure  the  desired packs by
editing the `PACKS` variable and run the  following to download them and
configure those that need to be configured.

    make packs

### Get JavaScript requirements

### Using the packages

If you are under the GNU/Linux distribution
of Arch Linux (or a derivative one) AUR packages
are available. Please have a look
[here](https://frnmst.github.io/swish-installer/)

### Without using the packages: building
#### Using bower

Install [bower](http://bower.io) for your  platform.   On  Ubuntu,  this
implies getting `node` and `npm` by installing two packages and next use
`npm` to install `bower`:

    sudo apt-get install npm nodejs-legacy
    sudo npm install -g bower

Once you have `bower`, run the following from the toplevel of `swish` to
get the dependencies:

    bower install
    make src

#### Download as zip

As installing node and bower is not a pleasure on all operating systems,
you can also download  the  dependencies  as   a  single  zip  file from
http://www.swi-prolog.org/download/swish/swish-bower-components.zip.
Unpack the zip file, maintaining the directory structure, from the swish
root directory to create the directory web/bower_components. If you have
`make` installed you can install the above `.zip` file using

    make bower-zip

Last updated: Apr 8, 2017: upgraded. Notably typeahead.js is forward nor
backward compatible and you need SWISH with commit
042d93a66409ef6460052c46394e4f83dcab3d90 (April 7, 2017) together with
this zip file.

### Get the latest SWI-Prolog

Install the latest  [SWI-Prolog](http://www.swi-prolog.org) _development
version_. As SWISH is very  much  in   flux  and  depends  on the recent
SWI-Prolog pengines and sandboxing libraries, it   is  quite common that
you            need            the             [nightly            build
(Windows)](http://www.swi-prolog.org/download/daily/bin/) or build   the
system    from    the     current      git     development    repository
[swipl-devel.git](https://github.com/SWI-Prolog/swipl-devel).

Jun 18, 2017: SWI-Prolog 7.5.9 works fine.

### Other dependencies

The   avatar   system   requires    the     `convert`    utility    from
[ImageMagic](http://www.imagemagick.org). This is available as a package
for virtually any Linux system, e.g., on Debian based systems do

    sudo apt-get install imagemagick

## Running SWISH

With a sufficiently recent Prolog installed, start the system by opening
`run.pl` either by running `swipl  run.pl`   (Unix)  or opening `run.pl`
from the Windows explorer.

Now direct your browser to http://localhost:3050/

If you want  to  know  what  the   latest  version  looks  like,  go  to
http://swish.swi-prolog.org/

### Configuring SWISH

There is a lot that can be configured in SWISH.  Roughly:

  - Make additional libraries available, e.g., RDF support, database
    connections, link to R, etc.

  - Configure authentication and authorization.  The default is not
    to demand and run commands sandboxed.  At the other extreme you
    can configure the system to demand login for all access and provide
    full access to Prolog.

Configuration is done  by  reading  `*.pl`   files  from  the  directory
`config-enabled`. The directory `config-available`   contains  templates
that can be copied and optionally edited to create a configuration.

See [README.md in
config-available](https://github.com/SWI-Prolog/swish/tree/master/config-available)
for details.


### Running SWISH without sandbox limitations

By default, SWISH does not require the user   to  login but lets you run
only _safe_ commands.  If  you  want   to  use  SWISH  for  unrestricted
development, enable the config file `auth_http_always.pl`:

    mkdir -p config-enabled
    (cd config-enabled && ln -s ../config-available/auth_http_always.pl)

Next, for first usage, you need  to   create  a user. The authentication
module defines swish_add_user/0, which asks for   details about the user
to be created and updates or  creates   a  file  called `passwd`. At the
moment _Group_ and _E-Mail_ are stored, but not used.

    ?- swish_add_user.
    % Password file: /home/jan/src/prolog/swish/passwd (update)
    User name: bob
    Real name: Bob Hacker
    Group:     user
    E-Mail:    bob@hacker.org
    Password:
    (again):
    true.

If you now try to run a command in  SWISH, it will prompt for a user and
password. After authentication you can run any Prolog predicate.

**NOTE** Authentication uses HTTP _digest   authentication_  by default.
This authentication method uses a   challenge-response  method to verify
the password and ensures the credentials  change with every request such
that old credentials cannot be re-used   by  an attacker. Unfortunately,
the server stores the password as the   SHA1 hash created from the user,
password and _realm_.  This  is   relatively  vulnerable  to brute-force
attacks for anyone who gains access to the  password file due to the low
computational overhead of SHA1 and the   lack of a user-specific _salt_.
Also note that the exchanged  commands   and  replies are not encrypted.
Secure servers should use HTTPS.

### Optional login

Instead of using `auth_http_always.pl` you can use `auth_http.pl`, which
allows for unauthenticated -sandboxed- usage as   well  as logging in to
the server and get unrestricted  access.   In  addition, several _social
login_ modules are provided to login  using Google, etc. Currently these
provide no additional rights. A more   fine grained authorization scheme
is planned.


## Running as a service

The script daemon.pl is provided to run SWISH  as a service or daemon on
Unix systems. Run this to get an overview of the options.

    ./daemon.pl --help

This script can be used to start  SWISH   as  a  daemon from the command
line, start SWISH from service managers   such as `upstart` or `systemd`
and    simplifies    running    as     an      HTTPS     server.     See
https://github.com/triska/letswicrypt.


## Design

Most of the application is realised  using client-side JavaScript, which
can be found  in  the  directory   `web/js`.  The  JavaScript  files use
[RequireJS](http://requirejs.org/)   for   dependency     tracking   and
[jQuery](http://jquery.com/) for structuring the   JavaScript  as jQuery
plugins. The accompanying CSS is in   `web/css`.  More details about the
organization of the JavaScript is in `web/js/README.md`

There are two overal pages. `web/swish.html`  provides a static page and
`lib/page.pl` provides a Prolog frontend to  generate the overal page or
parts thereof dynamically. The latter   facilitates smoothless embedding
in SWI-Prolog web applications.


## Development and debugging

No building is needed  to  run  the   system  from  sources.  For public
installations you probably want to create   the  minified JavaScript and
CSS files to reduce network traffic and startup time. You need some more
tools for that:

    % sudo npm install -g jsdoc
    % sudo npm install -g requirejs
    % sudo npm install -g clean-css-cli

You also need GNU make installed as   `make`  and SWI-Prolog as `swipl`.
With all that in  place,  the   following  command  creates the minified
versions:

    % make min

The default main page (`/`)  is   generated  from `lib/page.pl`. It uses
minified    JavaScript    and    CSS      from     `web/js/swish-min.js`
`web/css/swish-min.css` when available. If the   minified  files are not
present,  the  server  automatically  includes   the  full  source.  The
generated files may be removed using

    make clean

Alternatively, use of the minified  files   can  be  disable from Prolog
using this command and reloading the page:

    ?- debug(nominified).

## Documentation

The JavaScript is documented   using  [JsDoc](http://usejsdoc.org/). The
generated documentation is available in `web/js/doc/index.html`.

cplint and cplint_r related documentation is availble respectively
[here](https://github.com/friguzzi/cplint/blob/master/doc/help-cplint.pdf)
and [here](https://frnmst.github.io/cplint_r/)
