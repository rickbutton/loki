import { Loki } from "./loki.mjs";

async function getModuleBuffer(path) {
    return (await fetch(path)).arrayBuffer();
}

async function run() {
    const runtimePath = "examples/runtime.wasm";
    const modulePath = "examples/test.wasm";

    const runtimeBuffer = await getModuleBuffer(runtimePath);

    const loki = new Loki(runtimeBuffer);
    await loki.init(runtimeBuffer);

    const moduleBuffer = await getModuleBuffer(modulePath);
    const value = await loki.load(moduleBuffer);
    console.log(value);
}

run();
