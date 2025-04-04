import { Hono } from "hono";

type Bindings = {
  EQUINOX_API_KEY: string;
};

const app = new Hono<{ Bindings: Bindings }>();

app.use("*", async (c, next) => {
  const apiKey = c.req.header("X-API-Key");
  const validApiKey = c.env.EQUINOX_API_KEY;

  if (apiKey !== validApiKey) {
    return c.text("Unauthorized", 401);
  }

  await next();
});

app.get("/", (c) => {
  return c.text("Hello Hono!");
});

export default app;
