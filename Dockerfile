FROM frolvlad/alpine-ruby:latest

# RUN apk add --no-cache ruby-irb
RUN gem install rspec --no-document

RUN mkdir /test
WORKDIR /test
VOLUME "/test"

CMD ["rspec", "."]
