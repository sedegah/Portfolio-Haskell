FROM haskell:9.4

WORKDIR /app
COPY . .

# Let Stack install the right GHC version if needed
RUN stack setup --install-ghc
RUN stack build

EXPOSE 3000
CMD ["stack", "exec", "haskell-portfolio"]
