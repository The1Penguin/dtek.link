from haskell:9.4.4-slim as build
workdir /app

copy . ./

run cabal update && \
    cabal build -j4 && \
    cabal install --install-method=copy --installdir . --overwrite-policy=always

from debian:11.6-slim
workdir /app

run apt update && apt install -y libgmp10

copy --from=build /app/dtek.link .

env API=localhost

expose 3000
cmd "./dtek.link"
