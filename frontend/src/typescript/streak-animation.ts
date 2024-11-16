import { gsap } from "gsap";

class StreakAnimation extends HTMLElement {
    constructor() {
      super();
      this.attachShadow({ mode: "open" });
    }
  
    connectedCallback() {
      const streakCount = this.getAttribute("streak") ?? "3";
      this.shadowRoot.innerHTML = `
        <style>
          :host {
            display: block;
            width: 400px;
            height: 400px;
            overflow: hidden;
          }
          .stage {
            position: relative;
            width: 100%;
            height: 100%;
          }
          .emoji {
            font-size: 240px;
            opacity: 0;
            position: absolute;
            left: 50%;
            top: 50%;
            transform: translate(-50%, -50%) scale(0.1);
            transform-origin: center;
          }
          .sparkle {
            position: absolute;
            font-size: 24px;
            opacity: 0;
            left: 50%;
            top: 50%;
            transform: translate(-50%, -50%);
            transform-origin: center;
          }
          .streak-text {
            position: absolute;
            width: 100%;
            text-align: center;
            font-size: 48px;
            font-weight: 600;
            bottom: 40px;
            opacity: 0;
            transform: scale(0.7);
            transform-origin: center bottom;
          }
        </style>
        <div class="stage">
          <div class="emoji">🔥</div>
          ${Array.from({ length: 12 }).map(() => '<div class="sparkle">✨</div>').join('')}
          <div class="streak-text">${streakCount} DAYS!</div>
        </div>
      `;
      this.animate();
    }
  
    animate() {
      const emoji = this.shadowRoot.querySelector(".emoji");
      const sparkles = this.shadowRoot.querySelectorAll(".sparkle");
      const text = this.shadowRoot.querySelector(".streak-text");
      const tl = gsap.timeline();
  
      // 炎の登場（ゆっくりとフェードイン）
      tl.set(emoji, { opacity: 0, scale: 0.05 })
        .to(emoji, {
          opacity: 1,
          scale: 0.15,
          duration: 1.2,
          ease: "power1.inOut"
        })
        // ためる（少し縮む）
        .to(emoji, {
          scale: 0.12,
          duration: 0.8,
          ease: "power2.in"
        })
        // パン！と膨らむ
        .to(emoji, {
          scale: 0.8,
          duration: 0.3,
          ease: "power4.out"
        })
        // 少し戻る（反動）
        .to(emoji, {
          scale: 0.4,
          duration: 0.2,
          ease: "power1.out"
        })
        // 最終的なサイズに落ち着く
        .to(emoji, {
          scale: 0.5,
          duration: 0.4,
          ease: "elastic.out(1, 0.8)"
        });
  
      // キラキラエフェクト
      sparkles.forEach((sparkle, i) => {
        const angle = (i / sparkles.length) * Math.PI * 2;
        const distance = 100;
        const randomDelay = i * 0.08;
        
        // キラキラの初期配置
        tl.to(sparkle, {
          x: `+=${Math.cos(angle) * distance * 0.3}`,
          y: `+=${Math.sin(angle) * distance * 0.3}`,
          scale: 0.4,
          opacity: 0.4,
          duration: 0.6,
          ease: "power2.out"
        }, "-=2");
  
        // キラキラの継続的な動き
        gsap.to(sparkle, {
          keyframes: [
            {
              x: `+=${Math.cos(angle) * distance * 0.4}`,
              y: `+=${Math.sin(angle) * distance * 0.4}`,
              scale: 0.7,
              opacity: 0.8,
              duration: 1.5,
              ease: "power1.out"
            },
            {
              x: `+=${Math.cos(angle) * distance}`,
              y: `+=${Math.sin(angle) * distance}`,
              scale: 0.65,
              opacity: 0.4,
              duration: 1.5,
              ease: "power1.inOut"
            }
          ],
          repeat: -1,
          yoyo: true,
          delay: randomDelay
        });
      });
  
      // テキストのアニメーション
      tl.to(text, {
        opacity: 1,
        scale: 1,
        duration: 0.2,
        ease: "back.out(1.9)"
      }, "-=0.4");
  
      // 炎の継続的な揺らぎ
      gsap.to(emoji, {
        keyframes: [
          {
            y: "-=3",
            rotation: -2,
            duration: 1.6,
          },
          {
            y: "+=3",
            rotation: 2,
            duration: 1.6,
          }
        ],
        repeat: -1,
        yoyo: true,
        ease: "power1.in",
        delay: 2.8
      });
    }
  }
  
  customElements.define("streak-animation", StreakAnimation);