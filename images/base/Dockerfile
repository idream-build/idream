ARG VERSION
FROM ejconlon/idris2-custom:${VERSION}
RUN mkdir -p /root/.idream/bin
COPY cache/idream /root/.idream/bin
ENV PATH="/root/.idream/bin:${PATH}"
