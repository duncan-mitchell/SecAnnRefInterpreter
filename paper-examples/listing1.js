SecAnn <!CSRV * Message * CryptKey * Signature!>;
SecAnn <!PrivKey * PubKey * SymKey!> Extends <!CryptKey!>;
SecAnn <!Plaintext * Ciphertext!> Extends <!Message!>;

//Mocks for library functions
var window = {
	crypto: {
		subtle: {
			generateKey: function(alg, extractable, keyUsages){return true;},
			encrypt: function(alg, key, data){return data;},
			sign: function(alg, key, data){return data;}
		},
		getRandomValues: function(arr) { 			
			for (var i = 0; i < arr.length; i++) {
				arr[i] = 0;
			}
			return null;
		}
	}
}

function Uint8Array(size) {
 	var arr = [];
 	arr.length = size;
 	return arr;
 }

function TextEncoder() {
    this.text = "";
    this.encode = function(msg) {
        return msg;
    }
}

//helper function since object cloning in S5 is a little different
function clone(obj) {
	var copy = {};
	if (null == obj || "object" != typeof obj) { return obj; }
	Object.getOwnPropertyNames(obj).forEach(function (attr) {
        if (obj.hasOwnProperty(attr)) {
        	copy[attr] = clone(obj[attr]);
        }
	});
    return copy;
}

//Annotated WebCrypto Shim
window.oldCrypto = clone(window.crypto);
var wc = window.oldCrypto.subtle;
var grvShim = function(arr) {
  window.oldCrypto.getRandomValues(arr);
  arr as <!CSRV!>;
  return null;
};
var gkShim = function(alg, extractable, keyUsages) {
  var key = wc.generateKey(alg, extractable, keyUsages);
  if (/RSA|ECD/.test(alg.name)) {
    key.privateKey = key.privateKey as <!PrivKey * CSRV!>;
    key.publicKey = key.publicKey as <!PubKey * CSRV!>;
    return key;
  } else if(/AES|HMAC/.test(alg.name)) {
    return (key as <!SymKey * CSRV!>);
  } else { throw FailedSecurityCheck; }
};
var dkShim = function(alg :S ["public", <!PubKey!>],
    masterKey : <!PrivKey!>, derivedKeyAlg, extractable, keyUsages) {
  let key = wc.deriveKey(alg, masterKey, derivedKeyAlg, extractable, 
    keyUsages);
  return (key as <!SymKey!>);
};
var encShim = function(alg :S ["iv", <!CSRV!>], key, data) {
  if (/AES/.test(alg.name)) {
    (function(arg : <!SymKey!>) {})(key);
  } else if (/RSA/.test(alg.name)) {
    (function(arg : <!PubKey!>) {})(key);
  } else { throw FailedSecurityCheck; }
  var res = window.oldCrypto.subtle.encrypt(alg, key, data);
  return (cpAnn(data, res) drop <!Plaintext!>) as <!Ciphertext!>;
};
var decShim = function(alg :S ["iv", <!CSRV!>], key, data) {
  if (/AES/.test(alg.name)) {
    (function(arg : <!SymKey!>) {})(key);
  } else if (/RSA/.test(alg.name)) {
    (function(arg : <!PrivKey!>) {})(key);
  } else { throw FailedSecurityCheck; }
  var res = wc.decrypt(alg, key, data);
  return ((cpAnn(data, res) drop <!Ciphertext!>) as <!Plaintext!>);
};
var ikShim = function(type, key, alg, extractable, keyUsages) {
  var pubKey = wc.importKey(type, key, alg, extractable, keyUsages);
  return (pubKey as <!PubKey!>);
};
var sigShim = function(alg, key, data : <!Ciphertext!>) {
  if (/HMAC/.test(alg.name)) {
  	(function(arg : <!SymKey!>) {})(key)
  } else if (/RSA|ECDSA/.test(alg.name)) {
  	(function(arg : <!PrivKey!>) {})(key)
  }
  var sig = wc.sign(alg, key, data);
  return (sig as <!Signature!>);
}
var wcShim = { generateKey: gkShim, deriveKey: dkShim,
  encrypt: encShim, decrypt: decShim, importKey: ikShim, sign: sigShim};
Object.defineProperty(window.crypto, "subtle", {value: wcShim});
Object.defineProperty(window.crypto, "getRandomValues", {value: grvShim});

//Application Code (Listing 1)
var c = window.crypto.subtle;

var sign = function(msg) {
  var alg = { name: "HMAC", hash: {name: "SHA-256"} };
  var key = c.generateKey(alg, false, ["sign", "verify"]);
  return c.sign({ name: "HMAC" }, key, msg);
}

var encrypt = function(msg) {
  var iv = new Uint8Array(12);
  window.crypto.getRandomValues(iv);
  var alg = { name: 'AES-GCM', iv: iv };
  var key = c.generateKey({name: "AES-GCM",
      length: 256}, false, ["encrypt", "decrypt"]);
  return { iv: iv, ct: c.encrypt(alg, key, msg) };
}

var msg = new TextEncoder().encode("my message");
console.log({ct: encrypt(msg).ct, sig: sign(msg)});