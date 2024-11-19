import { gsap } from "gsap";

class BreathingAnimation extends HTMLElement {
  static get observedAttributes() {
    return [
      "inhale",
      "inhale-hold",
      "exhale",
      "exhale-hold",
      "paused",
      "duration",
    ];
  }

  constructor() {
    super();
    this.attachShadow({ mode: "open" });
    this.shadowRoot.innerHTML = `
      <style>
      :host {
        display: block;
      }
      .breathing-container {
        width: 100px;
        height: 100px;
        position: relative;
        transform-origin: center center;
        transform: scale(1);
        will-change: transform, border-radius;
        border-radius: 50%;
        margin: 0 auto;
      }
      .circle {
        width: 100%;
        height: 100%;
        position: absolute;
        top: 0;
        left: 0;
        border-radius: inherit;
        box-sizing: border-box;
      }
      .background {
        background: #fff;
        width: calc(100% - 10px); /* 幅を10px減らす（左右5pxずつ） */
        height: calc(100% - 10px); /* 高さを10px減らす（上下5pxずつ） */
        position: absolute;
        top: 5px; /* 上から5px下げる */
        left: 5px; /* 左から5px右にずらす */
        border-radius: inherit;
      }
      .progress {
        background: conic-gradient(#3498db var(--angle, 0deg), #ddd 0);
        width: 100%;
        height: 100%;
        position: absolute;
        top: 0;
        left: 0;
        border-radius: inherit;
      }
      </style>
      <div class="breathing-container">
        <div class="circle progress"></div>
        <div class="circle background"></div>
      </div>
    `;

    this.container = this.shadowRoot.querySelector(".breathing-container");
    this.progress = this.shadowRoot.querySelector(".progress");
    this.background = this.shadowRoot.querySelector(".background");
    this.timeline = null;
    this.startTime = null;
    this.progressTimeline = null;
  }

  connectedCallback() {
    this.setupAnimation();
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (oldValue === newValue) return;

    if (name === "duration") {
      this.setupProgressAnimation(parseInt(this.getAttribute("duration")));
    } else if (name === "paused") {
      this.timeline?.paused(newValue === "true");
      this.progressTimeline?.paused(newValue === "true");
    } else {
      // Reset animation when breathing pattern changes
      this.setupAnimation();
    }
  }

  setupProgressAnimation(duration) {
    if (this.progressTimeline) {
      this.progressTimeline.kill();
    }

    // プログレスのための新しいタイムライン
    this.progressTimeline = gsap.timeline({
      ease: "none",
    });

    // 0から360度までの回転を表現
    this.progressTimeline.to(this.progress, {
      duration: duration,
      onUpdate: () => {
        const angle = this.progressTimeline.progress() * 360;
        this.progress.style.setProperty("--angle", `${angle}deg`);
      },
    });

    if (this.getAttribute("paused") === "true") {
      this.progressTimeline.pause();
    }
  }

  setupAnimation() {
    if (this.timeline) {
      this.timeline.kill();
    }

    const inhale = parseInt(this.getAttribute("inhale")) ?? 4;
    const inhaleHold = parseInt(this.getAttribute("inhale-hold")) ?? 7;
    const exhale = parseInt(this.getAttribute("exhale")) ?? 8;
    const exhaleHold = parseInt(this.getAttribute("exhale-hold")) ?? 0;

    const SHAPE_CHANGE_DURATION = 0.25;

    this.timeline = gsap.timeline({
      repeat: -1,
      defaults: { ease: "power1.inOut" },
    });

    // Inhale
    this.timeline
      .to(
        this.container,
        {
          width: "250px",
          height: "250px",
          borderRadius: "50%",
          duration: inhale,
        },
        0
      )
      .to(
        this.background,
        {
          borderRadius: "50%",
          duration: inhale,
        },
        0
      );

    // Inhale Hold (if exists)
    if (inhaleHold > 0) {
      this.timeline
        .to(
          this.container,
          {
            borderRadius: "8px",
            duration: SHAPE_CHANGE_DURATION,
            ease: "power1.in",
          },
          inhale
        )
        .to(
          this.background,
          {
            borderRadius: "3px",
            duration: SHAPE_CHANGE_DURATION,
            ease: "power1.in",
          },
          inhale
        )
        .to(this.container, {
          duration: inhaleHold - SHAPE_CHANGE_DURATION * 2,
        })
        .to(
          this.container,
          {
            borderRadius: "50%",
            duration: SHAPE_CHANGE_DURATION,
            ease: "power1.in",
          },
          inhale + inhaleHold - SHAPE_CHANGE_DURATION
        )
        .to(
          this.background,
          {
            borderRadius: "50%",
            duration: SHAPE_CHANGE_DURATION,
            ease: "power1.in",
          },
          inhale + inhaleHold - SHAPE_CHANGE_DURATION
        );
    }

    // Exhale
    this.timeline
      .to(
        this.container,
        {
          width: "100px",
          height: "100px",
          borderRadius: "50%",
          duration: exhale,
        },
        inhale + inhaleHold
      )
      .to(
        this.background,
        { borderRadius: "50%", duration: exhale },
        inhale + inhaleHold
      );

    // Exhale Hold (if exists)
    if (exhaleHold > 0) {
      this.timeline
        .to(
          this.container,
          {
            borderRadius: "8px",
            duration: SHAPE_CHANGE_DURATION,
            ease: "power1.in",
          },
          inhale + inhaleHold + exhale
        )
        .to(
          this.background,
          {
            borderRadius: "3px",
            duration: SHAPE_CHANGE_DURATION,
            ease: "power1.in",
          },
          inhale + inhaleHold + exhale
        )
        .to(this.container, {
          duration: exhaleHold - SHAPE_CHANGE_DURATION * 2,
        })
        .to(
          this.container,
          {
            borderRadius: "50%",
            duration: SHAPE_CHANGE_DURATION,
            ease: "power1.in",
          },
          inhale + inhaleHold + exhale + exhaleHold - SHAPE_CHANGE_DURATION
        )
        .to(
          this.background,
          {
            borderRadius: "50%",
            duration: SHAPE_CHANGE_DURATION,
            ease: "power1.in",
          },
          inhale + inhaleHold + exhale + exhaleHold - SHAPE_CHANGE_DURATION
        );
    }

    // Check if should be paused
    if (this.getAttribute("paused") === "true") {
      this.timeline.pause();
    }
  }

  disconnectedCallback() {
    if (this.timeline) {
      this.timeline.kill();
    }
  }
}

customElements.define("breathing-animation", BreathingAnimation);
