import { Schwasm } from "./schwasm.mjs";
import * as fs from "fs";

function getModuleBuffer(path) {
    return fs.readFileSync(path);
}

async function run() {
    const runtimePath = process.argv[2];
    const modulePath = process.argv[3];

    const runtimeBuffer = getModuleBuffer(runtimePath);

    const schwasm = new Schwasm(runtimeBuffer);
    await schwasm.init(runtimeBuffer);

    const value = await schwasm.load(getModuleBuffer(modulePath));
    console.log(value);
}

run();
