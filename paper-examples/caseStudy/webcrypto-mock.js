//An internal crypto library
//A synchronous mock of necessary WebCrypto functionality 
//via Node's crypto module
var c = require('crypto');

var window = {
	crypto: {
		getRandomValues: function(array) {
			var rand = c.randomBytes(array.length);
			for (var i = 0; i < array.length; i++) {
				array[i] = rand[i];
			}
		},
		subtle: {
			generateKey: function(alg, extractable, keyUsages){
				//We mock WebCrypto's elliptic curve diffie-hellman with 
				//regular diffie-hellman for simplicity.
				if (alg.name == "ECDH") {
					var dh = c.createDiffieHellman(alg.generator[0], alg.prime[0]);
					var pub = dh.generateKeys('hex');
					var priv = dh.getPrivateKey('hex');
					return {
						alg: alg,
						extractable: extractable,
						keyUsages: keyUsages,
						publicKey: pub,
						privateKey: priv,
					}
				}
				return {
					alg: alg,
					extractable: extractable,
					keyUsages: keyUsages,
					key: new Uint8Array([alg.length])
				};
			},
			encrypt: function(alg, key, data) {
				if(alg.name == 'AES-CBC') {
					var iv = Buffer.from(alg.iv, 'hex');
					var cipher = c.createCipheriv('AES-128-CBC',
						key.key, iv)
					var enc = cipher.update(data, 'utf8', 'hex');
				 	enc += cipher.final('hex');
				 	return [enc, iv.toString('hex')];
				} else {
					throw 'invalid algorithm'
				}
			},
			decrypt: function(alg, key, data) {
				if(alg.name == 'AES-CBC') {
					var iv = Buffer.from(alg.iv, 'hex');
					var decipher = c.createDecipheriv('AES-128-CBC',
						key.key, iv);
					var dec = decipher.update(data, 'hex', 'utf8');
					dec +=decipher.final('utf8');
					return dec;
				} else {
					throw 'invalid algorithm'
				}
			},
			sign: function(alg, key, data){return data;},
			deriveKey: function(alg, masterKey, derivedKeyAlgorithm, extractable, keyUsages) {
				var dh = c.createDiffieHellman(alg.generator[0], alg.prime[0]);
				dh.setPrivateKey(masterKey, 'hex');
				var sharedSecret = dh.computeSecret(alg.public, 'hex');
				//padding to correct length
				var padded = new Uint8Array(16);
				padded[14] = sharedSecret[0];
				padded[15] = sharedSecret[1];
				return {
					alg: derivedKeyAlgorithm,
					key: padded,
					keyUsages: keyUsages,
					extractable: extractable
				}
			},
			importKey: function(type, key, alg, extractable, keyUsages) {
				return key;
			}
		}
	}
}

module.exports = {
	getRandomValues: function(array){
		window.crypto.getRandomValues(array);
		return null;
	},
	subtle: {
		generateKey: function(alg, extractable, keyUsages) {
	  		return window.crypto.subtle.generateKey(alg, extractable, keyUsages)
		},
		deriveKey: function(alg, masterKey, derivedKeyAlgorithm, extractable, keyUsages) {
			return window.crypto.subtle.deriveKey(alg, masterKey, derivedKeyAlgorithm, extractable, keyUsages);
		},
		encrypt: function(alg,
						  key,
						  data) {
  			return window.crypto.subtle.encrypt(alg, key, data);
		},
		decrypt: function(alg, key, data) {
  			return res = window.crypto.subtle.decrypt(alg, key, data);
		},
		sign: function(alg, 
					   key,
					   data) {
  			return window.crypto.subtle.sign(alg, key, data);
  		},
		importKey: function(type, key, alg, extractable, keyUsages) {
			return window.crypto.subtle.importKey(type, key, alg, extractable, keyUsages);
		}
	}
}