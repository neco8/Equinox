import { v4 } from "uuid";
import { Elm } from "../Main.elm";

export const applyUuid = (elm: Elm.Main.App): Elm.Main.App => {
  elm.ports.generateUuid.subscribe((tag) => {
    elm.ports.receiveUuid.send([tag, v4()]);
  });
  return elm;
};
