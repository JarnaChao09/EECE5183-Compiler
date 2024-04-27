# syntax=docker/dockerfile:1
FROM homebrew/brew
WORKDIR /src
RUN mkdir stuff
WORKDIR /src/stuff
COPY . .
RUN sudo chmod -R 777 *
ENV HOMEBREW_NO_AUTO_UPDATE=1
RUN mkdir -p /home/linuxbrew/.linuxbrew/Cellar/llvm/18.1.4/bin
RUN brew install llvm@17
RUN brew pin llvm@17
ENV HOMEBREW_NO_AUTO_UPDATE=1
ENV LDFLAGS="-L/home/linuxbrew/.linuxbrew/opt/llvm@17/lib"
ENV LD_LIBRARY_PATH="/home/linuxbrew/.linuxbrew/opt/llvm@17/lib:/home/linuxbrew/.linuxbrew/lib"
ENV PATH="/home/linuxbrew/.linuxbrew/opt/llvm@17/bin:/home/linuxbrew/.linuxbrew/bin:$PATH"
RUN mkdir tmp
WORKDIR /src/stuff/tmp
RUN curl -o crystal.rb https://raw.githubusercontent.com/Homebrew/homebrew-core/337abdc50d8299795a13edf78b5eb8f340985579/Formula/c/crystal.rb
RUN brew install --formula crystal.rb; exit 0
WORKDIR /src/stuff
RUN shards build compiler -O3