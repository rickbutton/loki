# install node/npm first
curl https://sh.rustup.rs -sSf | sh
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh 
source $HOME/.cargo/env

# to generate a project
# cargo install cargo-generate
# sudo apt-get install pkg-config libssl-dev cmake

rustup toolchain install nightly
rustup target add wasm32-unknown-unknown --toolchain nightly