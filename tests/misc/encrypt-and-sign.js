//Security Declarations
SecAnn <!CryptKey * Plaintext * Ciphertext * Signature!>;

//INTERNAL METHODS --- MOCKED
var internal = {
	crypto: {
		subtle: {
			generateKey: function(alg, extractable, keyUsages){return true;},
			encrypt: function(alg, key, data){return data;},
			sign: function(alg, key, data){return data;}
		}
	}
}

function Uint8Array(int) {
    this.val = int;
}

function TextEncoder() {
    this.text = "";
    this.encode = function(msg) {
        return msg;
    }
}

//ANNOTATED LIBRARY
var crypto = {
	getRandomValues: function(array){return true;},
	subtle: {
		generateKey: function(alg, extractable, keyUsages) {
  			return internal.crypto.subtle.generateKey(alg, extractable, keyUsages) as <!CryptKey!>;
		},
		encrypt: function(alg, key : <!CryptKey!>, data) {
  			var res = internal.crypto.subtle.encrypt(alg, key, data);
  			return cpAnn(data, res) drop <!Plaintext!> as <!Ciphertext!>;
		},
		sign: function(alg, key : <!CryptKey!>, data : <!Ciphertext!>) {
  			return internal.crypto.subtle.sign(alg, key, data) as <!Signature!>;
		}
	}
}

//END OF ANNOTATED LIBRARY
var c = crypto.subtle;

var sign = function(msg) {
  var alg = { name: "HMAC", hash: {name: "SHA-256"} };
  var key = c.generateKey(alg, false, ["sign", "verify"]);
  return c.sign({ name: "HMAC" }, key, msg);
}

var encrypt = function(msg) {
  var iv = crypto.getRandomValues(new Uint8Array(12));
  var alg = { name: 'AES-GCM', iv: iv };
  var key = c.generateKey({name: "AES-GCM",
      length: 256}, false, ["encrypt", "decrypt"]);
  return { iv: iv, ct: c.encrypt(alg, key, msg) };
}

var msg = new TextEncoder().encode("my message");
console.log({ct: encrypt(msg).ct, sig: sign(msg)});
