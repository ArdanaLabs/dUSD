(()=>{var e={"./node_modules/@trezor/transport/lib/utils/defered.js":(e,t,n)=>{"use strict";var r=n("./node_modules/es6-promise/dist/es6-promise.js").Promise;Object.defineProperty(t,"__esModule",{value:!0}),t.resolveTimeoutPromise=t.create=void 0,t.create=function(){let e=()=>{},t=()=>{};const n=new r(((n,r)=>{e=n,t=r})),o=n.then((()=>{throw new Error("Promise is always rejecting")}));return o.catch((()=>{})),{resolve:e,reject:t,promise:n,rejectingPromise:o}},t.resolveTimeoutPromise=function(e,t){return new r((n=>{setTimeout((()=>{n(t)}),e)}))}},"./node_modules/es6-promise/dist/es6-promise.js":function(e,t,n){var r=n("./node_modules/process/browser.js");
/*!
 * @overview es6-promise - a tiny implementation of Promises/A+.
 * @copyright Copyright (c) 2014 Yehuda Katz, Tom Dale, Stefan Penner and contributors (Conversion to ES6 API by Jake Archibald)
 * @license   Licensed under MIT license
 *            See https://raw.githubusercontent.com/stefanpenner/es6-promise/master/LICENSE
 * @version   v4.2.8+1e68dce6
 */e.exports=function(){"use strict";function e(e){return"function"==typeof e}var t=Array.isArray?Array.isArray:function(e){return"[object Array]"===Object.prototype.toString.call(e)},o=0,i=void 0,s=void 0,u=function(e,t){p[o]=e,p[o+1]=t,2===(o+=2)&&(s?s(v):y())};var c="undefined"!=typeof window?window:void 0,a=c||{},l=a.MutationObserver||a.WebKitMutationObserver,f="undefined"==typeof self&&void 0!==r&&"[object process]"==={}.toString.call(r),h="undefined"!=typeof Uint8ClampedArray&&"undefined"!=typeof importScripts&&"undefined"!=typeof MessageChannel;function d(){var e=setTimeout;return function(){return e(v,1)}}var p=new Array(1e3);function v(){for(var e=0;e<o;e+=2)(0,p[e])(p[e+1]),p[e]=void 0,p[e+1]=void 0;o=0}var y=void 0;function m(e,t){var n=this,r=new this.constructor(w);void 0===r[g]&&M(r);var o=n._state;if(o){var i=arguments[o-1];u((function(){return x(o,r,i,n._result)}))}else P(n,r,e,t);return r}function _(e){if(e&&"object"==typeof e&&e.constructor===this)return e;var t=new this(w);return j(t,e),t}y=f?function(){return r.nextTick(v)}:l?function(){var e=0,t=new l(v),n=document.createTextNode("");return t.observe(n,{characterData:!0}),function(){n.data=e=++e%2}}():h?function(){var e=new MessageChannel;return e.port1.onmessage=v,function(){return e.port2.postMessage(0)}}():void 0===c?function(){try{var e=Function("return this")().require("vertx");return void 0!==(i=e.runOnLoop||e.runOnContext)?function(){i(v)}:d()}catch(e){return d()}}():d();var g=Math.random().toString(36).substring(2);function w(){}var b=void 0;function T(t,n,r){n.constructor===t.constructor&&r===m&&n.constructor.resolve===_?function(e,t){1===t._state?E(e,t._result):2===t._state?S(e,t._result):P(t,void 0,(function(t){return j(e,t)}),(function(t){return S(e,t)}))}(t,n):void 0===r?E(t,n):e(r)?function(e,t,n){u((function(e){var r=!1,o=function(e,t,n,r){try{e.call(t,n,r)}catch(e){return e}}(n,t,(function(n){r||(r=!0,t!==n?j(e,n):E(e,n))}),(function(t){r||(r=!0,S(e,t))}),e._label);!r&&o&&(r=!0,S(e,o))}),e)}(t,n,r):E(t,n)}function j(e,t){if(e===t)S(e,new TypeError("You cannot resolve a promise with itself"));else if(function(e){var t=typeof e;return null!==e&&("object"===t||"function"===t)}(t)){var n=void 0;try{n=t.then}catch(t){return void S(e,t)}T(e,t,n)}else E(e,t)}function A(e){e._onerror&&e._onerror(e._result),k(e)}function E(e,t){e._state===b&&(e._result=t,e._state=1,0!==e._subscribers.length&&u(k,e))}function S(e,t){e._state===b&&(e._state=2,e._result=t,u(A,e))}function P(e,t,n,r){var o=e._subscribers,i=o.length;e._onerror=null,o[i]=t,o[i+1]=n,o[i+2]=r,0===i&&e._state&&u(k,e)}function k(e){var t=e._subscribers,n=e._state;if(0!==t.length){for(var r=void 0,o=void 0,i=e._result,s=0;s<t.length;s+=3)r=t[s],o=t[s+n],r?x(n,r,o,i):o(i);e._subscribers.length=0}}function x(t,n,r,o){var i=e(r),s=void 0,u=void 0,c=!0;if(i){try{s=r(o)}catch(t){c=!1,u=t}if(n===s)return void S(n,new TypeError("A promises callback cannot return that same promise."))}else s=o;n._state!==b||(i&&c?j(n,s):!1===c?S(n,u):1===t?E(n,s):2===t&&S(n,s))}var O=0;function M(e){e[g]=O++,e._state=void 0,e._result=void 0,e._subscribers=[]}var C=function(){function e(e,n){this._instanceConstructor=e,this.promise=new e(w),this.promise[g]||M(this.promise),t(n)?(this.length=n.length,this._remaining=n.length,this._result=new Array(this.length),0===this.length?E(this.promise,this._result):(this.length=this.length||0,this._enumerate(n),0===this._remaining&&E(this.promise,this._result))):S(this.promise,new Error("Array Methods must be provided an Array"))}return e.prototype._enumerate=function(e){for(var t=0;this._state===b&&t<e.length;t++)this._eachEntry(e[t],t)},e.prototype._eachEntry=function(e,t){var n=this._instanceConstructor,r=n.resolve;if(r===_){var o=void 0,i=void 0,s=!1;try{o=e.then}catch(e){s=!0,i=e}if(o===m&&e._state!==b)this._settledAt(e._state,t,e._result);else if("function"!=typeof o)this._remaining--,this._result[t]=e;else if(n===L){var u=new n(w);s?S(u,i):T(u,e,o),this._willSettleAt(u,t)}else this._willSettleAt(new n((function(t){return t(e)})),t)}else this._willSettleAt(r(e),t)},e.prototype._settledAt=function(e,t,n){var r=this.promise;r._state===b&&(this._remaining--,2===e?S(r,n):this._result[t]=n),0===this._remaining&&E(r,this._result)},e.prototype._willSettleAt=function(e,t){var n=this;P(e,void 0,(function(e){return n._settledAt(1,t,e)}),(function(e){return n._settledAt(2,t,e)}))},e}();var L=function(){function t(e){this[g]=O++,this._result=this._state=void 0,this._subscribers=[],w!==e&&("function"!=typeof e&&function(){throw new TypeError("You must pass a resolver function as the first argument to the promise constructor")}(),this instanceof t?function(e,t){try{t((function(t){j(e,t)}),(function(t){S(e,t)}))}catch(t){S(e,t)}}(this,e):function(){throw new TypeError("Failed to construct 'Promise': Please use the 'new' operator, this object constructor cannot be called as a function.")}())}return t.prototype.catch=function(e){return this.then(null,e)},t.prototype.finally=function(t){var n=this,r=n.constructor;return e(t)?n.then((function(e){return r.resolve(t()).then((function(){return e}))}),(function(e){return r.resolve(t()).then((function(){throw e}))})):n.then(t,t)},t}();return L.prototype.then=m,L.all=function(e){return new C(this,e).promise},L.race=function(e){var n=this;return t(e)?new n((function(t,r){for(var o=e.length,i=0;i<o;i++)n.resolve(e[i]).then(t,r)})):new n((function(e,t){return t(new TypeError("You must pass an array to race."))}))},L.resolve=_,L.reject=function(e){var t=new this(w);return S(t,e),t},L._setScheduler=function(e){s=e},L._setAsap=function(e){u=e},L._asap=u,L.polyfill=function(){var e=void 0;if(void 0!==n.g)e=n.g;else if("undefined"!=typeof self)e=self;else try{e=Function("return this")()}catch(e){throw new Error("polyfill failed because global object is unavailable in this environment")}var t=e.Promise;if(t){var r=null;try{r=Object.prototype.toString.call(t.resolve())}catch(e){}if("[object Promise]"===r&&!t.cast)return}e.Promise=L},L.Promise=L,L}()},"./node_modules/process/browser.js":e=>{var t,n,r=e.exports={};function o(){throw new Error("setTimeout has not been defined")}function i(){throw new Error("clearTimeout has not been defined")}function s(e){if(t===setTimeout)return setTimeout(e,0);if((t===o||!t)&&setTimeout)return t=setTimeout,setTimeout(e,0);try{return t(e,0)}catch(n){try{return t.call(null,e,0)}catch(n){return t.call(this,e,0)}}}!function(){try{t="function"==typeof setTimeout?setTimeout:o}catch(e){t=o}try{n="function"==typeof clearTimeout?clearTimeout:i}catch(e){n=i}}();var u,c=[],a=!1,l=-1;function f(){a&&u&&(a=!1,u.length?c=u.concat(c):l=-1,c.length&&h())}function h(){if(!a){var e=s(f);a=!0;for(var t=c.length;t;){for(u=c,c=[];++l<t;)u&&u[l].run();l=-1,t=c.length}u=null,a=!1,function(e){if(n===clearTimeout)return clearTimeout(e);if((n===i||!n)&&clearTimeout)return n=clearTimeout,clearTimeout(e);try{n(e)}catch(t){try{return n.call(null,e)}catch(t){return n.call(this,e)}}}(e)}}function d(e,t){this.fun=e,this.array=t}function p(){}r.nextTick=function(e){var t=new Array(arguments.length-1);if(arguments.length>1)for(var n=1;n<arguments.length;n++)t[n-1]=arguments[n];c.push(new d(e,t)),1!==c.length||a||s(h)},d.prototype.run=function(){this.fun.apply(null,this.array)},r.title="browser",r.browser=!0,r.env={},r.argv=[],r.version="",r.versions={},r.on=p,r.addListener=p,r.once=p,r.off=p,r.removeListener=p,r.removeAllListeners=p,r.emit=p,r.prependListener=p,r.prependOnceListener=p,r.listeners=function(e){return[]},r.binding=function(e){throw new Error("process.binding is not supported")},r.cwd=function(){return"/"},r.chdir=function(e){throw new Error("process.chdir is not supported")},r.umask=function(){return 0}}},t={};function n(r){var o=t[r];if(void 0!==o)return o.exports;var i=t[r]={exports:{}};return e[r].call(i.exports,i,i.exports,n),i.exports}n.g=function(){if("object"==typeof globalThis)return globalThis;try{return this||new Function("return this")()}catch(e){if("object"==typeof window)return window}}(),(()=>{"use strict";var e=n("./node_modules/es6-promise/dist/es6-promise.js").Promise;const t=n("./node_modules/@trezor/transport/lib/utils/defered.js"),r={},o={};let i=null,s=e.resolve();function u(){const e=(0,t.create)();i=e,setTimeout((()=>e.reject(new Error("Timed out"))),1e4)}function c(e){null!=i&&i.resolve(e)}function a(){return null==i?e.reject(new Error("???")):i.promise}function l(e){const t=s.then((()=>e()));s=t.catch((()=>{}))}function f(e,t,n){n.postMessage({id:t,message:e})}function h(e){c({id:e})}function d(t,n,i){if(null!=i){const e={};i.forEach((t=>{e[t.path]=!0})),Object.keys(r).forEach((e=>{r[e]||delete r[e]})),Object.keys(o).forEach((e=>{o[e]||delete o[e]}))}return f({type:"sessions",debugSessions:o,normalSessions:r},t,n),e.resolve()}let p=0;function v({id:t,message:n},i){if("acquire-intent"===n.type){const{path:s}=n,{previous:c}=n,{debug:h}=n;l((()=>function(t,n,i,s,c){let l=!1;const h=c?o:r,d=c?r:o,v=h[t];return l=null==v?null!=s:s!==v,l?(f({type:"wrong-previous-session"},n,i),e.resolve()):(u(),f({type:"other-session",otherSession:d[t]},n,i),a().then((e=>{if(e.good){p++;let n=p.toString();c&&(n=`debug${n}`),h[t]=n,f({type:"session-number",number:n},e.id,i)}else f({type:"ok"},e.id,i)})))}(s,t,i,c,h)))}if("acquire-done"===n.type&&function(e){c({good:!0,id:e})}(t),"acquire-failed"===n.type&&function(e){c({good:!1,id:e})}(t),"get-sessions"===n.type&&l((()=>d(t,i))),"get-sessions-and-disconnect"===n.type){const{devices:e}=n;l((()=>d(t,i,e)))}if("release-onclose"===n.type){const{session:t}=n;l((()=>function(t){let n=null;if(Object.keys(r).forEach((e=>{r[e]===t&&(n=e)})),null==n)return e.resolve();const i=n;return delete r[i],delete o[i],e.resolve()}(t)))}if("release-intent"===n.type){const{session:s}=n,{debug:c}=n;l((()=>function(t,n,i,s){let c=null;const l=n?o:r,h=n?r:o;if(Object.keys(l).forEach((e=>{l[e]===t&&(c=e)})),null==c)return f({type:"double-release"},i,s),e.resolve();const d=c,p=h[d];return u(),f({type:"path",path:d,otherSession:p},i,s),a().then((e=>{delete l[d],f({type:"ok"},e.id,s)}))}(s,c,t,i)))}"release-done"===n.type&&h(t),"enumerate-intent"===n.type&&l((()=>function(e,t){return u(),f({type:"ok"},e,t),a().then((e=>{f({type:"ok"},e.id,t)}))}(t,i))),"enumerate-done"===n.type&&h(t)}"undefined"!=typeof onconnect&&(onconnect=function(e){const t=e.ports[0];t.onmessage=function(e){v(e.data,t)}})})()})();