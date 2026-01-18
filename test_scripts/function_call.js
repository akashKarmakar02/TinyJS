function getThis() {
    return this;
}

console.log("call_undefined_is_global =", getThis.call(undefined) === globalThis);

function getX() {
    return this.x;
}

let o = { x: 7 };
console.log("getX.call(o) =", getX.call(o));

o.getX = getX;
console.log("method_call_getX =", o.getX());

