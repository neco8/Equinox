import { gsap } from "gsap";

class BreathingAnimation extends HTMLElement {
  static get observedAttributes() {
    return ["inhale", "inhale-hold", "exhale", "exhale-hold", "paused"];
  }

  constructor() {
    super();
    this.attachShadow({ mode: "open" });
    this.shadowRoot.innerHTML = `
        <style>
          :host {
            display: block;
          }
          .breathing-circle {
            width: 100px;
            height: 100px;
            border: 5px solid #3498db;
            border-radius: 50%;
            display: block;
            transform-origin: center center;
            position: relative;
            transform: scale(1);
            will-change: transform, border-radius;
            margin: 0 auto;
          }
        </style>
        <div class="breathing-circle"></div>
      `;

    this.circle = this.shadowRoot.querySelector(".breathing-circle");
    this.timeline = null;
  }

  connectedCallback() {
    this.setupAnimation();
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (oldValue === newValue) return;

    if (name === "paused") {
      this.timeline?.paused(newValue === "true");
    } else {
      // Reset animation when breathing pattern changes
      this.setupAnimation();
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

    const SHAPE_CHANGE_DURATION = 0.5;

    this.timeline = gsap.timeline({
      repeat: -1,
      defaults: { ease: "power1.inOut" },
    });

    // Inhale
    this.timeline.to(this.circle, {
      width: "250px",
      height: "250px",
      borderRadius: "50%",
      duration: inhale,
    });

    // Inhale Hold (if exists)
    if (inhaleHold > 0) {
      this.timeline
        .to(this.circle, {
          borderRadius: "3%",
          duration: SHAPE_CHANGE_DURATION,
          ease: "power1.in",
        })
        .to(this.circle, {
          duration: inhaleHold - SHAPE_CHANGE_DURATION * 2,
        })
        .to(this.circle, {
          borderRadius: "50%",
          duration: SHAPE_CHANGE_DURATION,
          ease: "power1.in",
        });
    }

    // Exhale
    this.timeline.to(this.circle, {
      width: "100px",
      height: "100px",
      borderRadius: "50%",
      duration: exhale,
    });

    // Exhale Hold (if exists)
    if (exhaleHold > 0) {
      this.timeline
        .to(this.circle, {
          borderRadius: "2%",
          duration: SHAPE_CHANGE_DURATION,
          ease: "power1.in",
        })
        .to(this.circle, {
          duration: exhaleHold - SHAPE_CHANGE_DURATION * 2,
        })
        .to(this.circle, {
          borderRadius: "50%",
          duration: SHAPE_CHANGE_DURATION,
          ease: "power1.in",
        });
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
