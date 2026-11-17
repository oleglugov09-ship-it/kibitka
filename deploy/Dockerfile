# Built from the game directory: fly deploy --remote-only --ha=false (after make web)
FROM nginx:1.27-alpine
# The root holds the page about the game (site/), the game itself is in /play/
COPY site /site
COPY export/web /site/play
RUN gzip -9 -k /site/play/*.wasm /site/play/*.pck /site/play/*.js
COPY deploy/nginx.conf /etc/nginx/conf.d/default.conf
