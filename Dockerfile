FROM haskell:9.2

# Set working directory
WORKDIR /app

# Copy project files
COPY . .

# Install dependencies and build
RUN stack setup
RUN stack build

# Expose port
EXPOSE 3000

# Run the application
CMD ["stack", "exec", "haskell-portfolio"]
