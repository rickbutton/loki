import { Loki } from "./loki.mjs";
import * as fs from "fs";

function getModuleBuffer(path) {
    return fs.readFileSync(path);
}

async function run() {
    const modulePath = process.argv[2];

    const loki = new Loki();
    const value = await loki.load(getModuleBuffer(modulePath));
    console.log(value);
}

run();
