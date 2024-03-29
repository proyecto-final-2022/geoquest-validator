FROM haskell:8.10.7

RUN apt install libpcre3 libpcre3-dev

COPY ["./stack.yaml", "./package.yaml", "./app/"]

WORKDIR /app

RUN stack build --only-dependencies

COPY . .

RUN stack build

ENTRYPOINT stack exec geoquest-validator-exe
