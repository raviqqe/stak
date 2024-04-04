import init, { compile } from "@raviqqe/stak";
import { handleWorkerMessage } from "../handle-worker-message";

handleWorkerMessage(init, compile);
