# Copyright 2022 ultasun. All rights reserved.
# See the LICENSE. Thank you for reading!

FROM alpine:latest
RUN apk -U upgrade && apk add clisp
RUN adduser -h /app -s /bin/sh -D -u 11111 foodstats
WORKDIR /app
COPY . .
RUN chown -R foodstats /app
USER 11111
CMD ["./run.sh"]
