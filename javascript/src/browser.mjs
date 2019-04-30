import { Schwasm } from "./schwasm.mjs";

async function getModuleBuffer(path) {
    return (await fetch(path)).arrayBuffer();
}

async function run() {
    const runtimePath = "examples/runtime.wasm";
    const modulePath = "examples/test.wasm";

    const runtimeBuffer = await getModuleBuffer(runtimePath);

    const schwasm = new Schwasm(runtimeBuffer);
    await schwasm.init(runtimeBuffer);

    const moduleBuffer = await getModuleBuffer(modulePath);
    const value = await schwasm.load(moduleBuffer);
    console.log(value);
}

run();