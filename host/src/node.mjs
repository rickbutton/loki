import { Loki } from "./loki.mjs";
import * as fs from "fs";

function getModuleBuffer(path) {
    return fs.readFileSync(path);
}

async function run() {
    const runtimePath = process.argv[2];
    const modulePath = process.argv[3];

    const runtimeBuffer = getModuleBuffer(runtimePath);

    const loki = new Loki(runtimeBuffer);
    await loki.init(runtimeBuffer);

    const value = await loki.load(getModuleBuffer(modulePath));
    console.log(value);
}

run();
