class LockScreenElement extends HTMLElement {
  constructor() {
    super();
    this.lock = null; // Lock APIハンドル
  }

  async connectedCallback() {
    if ("wakeLock" in navigator) {
      try {
        this.lock = await navigator.wakeLock.request("screen");
        console.log("Screen lock acquired");
      } catch (err) {
        console.error("Screen lock failed:", err);
      }
    } else {
      console.warn("Wake Lock API not supported");
    }
  }

  disconnectedCallback() {
    if (this.lock) {
      this.lock.release().then(() => {
        console.log("Screen lock released");
        this.lock = null;
      });
    }
  }
}

customElements.define("lock-screen", LockScreenElement);
