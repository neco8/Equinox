import { v4 } from "uuid";
import { Elm } from "../Main.elm";

export const applyUuid = (elm: Elm.Main.App): Elm.Main.App => {
  elm.ports.generateUuid.subscribe(() => {
    elm.ports.receiveUuid.send(v4());
  });
  return elm;
};
