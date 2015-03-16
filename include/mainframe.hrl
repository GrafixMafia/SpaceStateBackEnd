% This is a rebuild of the space api data structure that can be found here: 
% http://spaceapi.net/documentation

% body
-record(space, {
          apiVersion,
          name,
          logo,
          url,
          location,
          spacenet,
          cams,
          streams,
          state,
          events,
          contact,
          issue_report_channels,
          sensors,
          feeds,
          cache,
          projects,
          radioshows
}).

% location
-record(location, {
          address,
          latitude,
          longitude
}).

% spacenet
-record(spacenet, {
          spacenet,
          spacesaml,
          spacephone
}).

% stream
-record(stream, {
          m4,
          mjpeg,
          ustream
}).

% state
-record(state, {
          open,
          lastchange,
          triggerperson,
          message,
          icon
}).

% ! MISSING 
% events,
% contact,
% issue_report_channels,
% sensors,
% feeds,
% cache,
% projects,

% radioshow
-record(radioshow, {
          name,
          url,
          type,
          starter,
          ender
}).