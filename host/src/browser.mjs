import { Loki } from "./loki.mjs";

async function getModuleBuffer(path) {
    return (await fetch(path)).arrayBuffer();
}

async function run() {
    const modulePath = "examples/test.wasm";

    const loki = new Loki();
    const moduleBuffer = await getModuleBuffer(modulePath);
    const value = await loki.load(moduleBuffer);
    console.log(value);
}

run();
