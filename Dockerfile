FROM haskell:9.4

WORKDIR /app
COPY . .


RUN stack setup --install-ghc
RUN stack build

EXPOSE 3000
CMD ["stack", "exec", "haskell-portfolio"]
