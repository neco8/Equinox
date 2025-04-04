import { Hono } from "hono";
import { cors } from "hono/cors";

import z from "zod";
import { drizzle } from "drizzle-orm/d1";
import { breathingMethods, NewBreathingMethod } from "./db/schema";
import { and, eq } from "drizzle-orm";
import { zValidator } from "@hono/zod-validator";

type Bindings = {
  EQUINOX_API_KEY: string;
  DB: D1Database;
};

const app = new Hono<{ Bindings: Bindings }>();

app.use(
  "*",
  cors({
    origin: "http://localhost:3000", // Elmアプリのオリジンに合わせる（または '*'）
    allowMethods: ["GET", "POST", "PUT", "DELETE", "OPTIONS"],
    credentials: true, // 必要なら
  })
);

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

// バリデーションスキーマ
const breathingMethodSchema = z.object({
  name: z.string().min(1).max(20),
  inhale: z.number().int().min(1).max(600), // 10分 = 600秒
  inhaleHold: z.number().int().min(0).max(600),
  exhale: z.number().int().min(1).max(600),
  exhaleHold: z.number().int().min(0).max(600),
});

// ルートパスのプレフィックス
const breathingMethodsApp = app.basePath("/breathing-methods");

// 全ての呼吸法を取得
breathingMethodsApp.get("/", async (c) => {
  const db = drizzle(c.env.DB);
  const results = await db.select().from(breathingMethods);

  return c.json(
    results.map((record) => ({
      id: record.id,
      name: record.name,
      inhale: record.inhale,
      inhaleHold: record.inhaleHold,
      exhale: record.exhale,
      exhaleHold: record.exhaleHold,
    }))
  );
});

// 特定のIDの呼吸法を取得
breathingMethodsApp.get("/:id", async (c) => {
  const id = c.req.param("id");
  const db = drizzle(c.env.DB);

  const [result] = await db
    .select()
    .from(breathingMethods)
    .where(eq(breathingMethods.id, id));

  if (!result) {
    return c.json({ error: "Breathing method not found" }, 404);
  }

  return c.json({
    id: result.id,
    name: result.name,
    inhale: result.inhale,
    inhaleHold: result.inhaleHold,
    exhale: result.exhale,
    exhaleHold: result.exhaleHold,
  });
});

// 新しい呼吸法を作成
breathingMethodsApp.post(
  "/",
  zValidator("json", breathingMethodSchema),
  async (c) => {
    const data = c.req.valid("json");
    const db = drizzle(c.env.DB);

    const id = crypto.randomUUID();

    const newBreathingMethod: NewBreathingMethod = {
      id,
      name: data.name,
      inhale: data.inhale,
      inhaleHold: data.inhaleHold,
      exhale: data.exhale,
      exhaleHold: data.exhaleHold,
    };

    await db.insert(breathingMethods).values(newBreathingMethod);

    return c.json({ ...data }, 201);
  }
);

// 呼吸法を更新
breathingMethodsApp.put(
  "/:id",
  zValidator("json", breathingMethodSchema),
  async (c) => {
    const id = c.req.param("id");
    const data = c.req.valid("json");
    const db = drizzle(c.env.DB);

    // 存在確認
    const [existing] = await db
      .select()
      .from(breathingMethods)
      .where(eq(breathingMethods.id, id));

    if (!existing) {
      return c.json({ error: "Breathing method not found" }, 404);
    }

    // 更新実行
    await db
      .update(breathingMethods)
      .set({
        name: data.name,
        inhale: data.inhale,
        inhaleHold: data.inhaleHold,
        exhale: data.exhale,
        exhaleHold: data.exhaleHold,
      })
      .where(eq(breathingMethods.id, id));

    return c.json({ ...data });
  }
);

// 呼吸法を削除
breathingMethodsApp.delete("/:id", async (c) => {
  const id = c.req.param("id");
  const db = drizzle(c.env.DB);

  // 存在確認
  const [existing] = await db
    .select()
    .from(breathingMethods)
    .where(eq(breathingMethods.id, id));

  if (!existing) {
    return c.json({ error: "Breathing method not found" }, 404);
  }

  // 削除実行
  await db.delete(breathingMethods).where(eq(breathingMethods.id, id));

  return c.json({ success: true });
});

// 重複確認用スキーマ
const breathingDuplicateSchema = z.object({
  inhale: z.number().int().min(1).max(600),
  inhaleHold: z.number().int().min(0).max(600),
  exhale: z.number().int().min(1).max(600),
  exhaleHold: z.number().int().min(0).max(600),
});

// 重複確認
breathingMethodsApp.post(
  "/check-duplicate",
  zValidator("json", breathingDuplicateSchema),
  async (c) => {
    const { inhale, inhaleHold, exhale, exhaleHold } = c.req.valid("json");
    const db = drizzle(c.env.DB);

    const results = await db
      .select()
      .from(breathingMethods)
      .where(
        and(
          eq(breathingMethods.inhale, inhale),
          eq(breathingMethods.inhaleHold, inhaleHold),
          eq(breathingMethods.exhale, exhale),
          eq(breathingMethods.exhaleHold, exhaleHold)
        )
      );

    const duplicate = results.length > 0;

    return c.json({ duplicate });
  }
);

export default app;
