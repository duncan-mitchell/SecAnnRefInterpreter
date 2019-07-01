/*
 * Mock for WebCrypto
 */
var window = {
	crypto: {
		getRandomValues: function(array) {
			for (var i = 0; i < array.length; i++) {
				array[i] = 0;
			}
			return null;
		},
		subtle: {
			generateKey: function(alg, extractable, keyUsages){
				if (alg.name == "ECDH") {
					var pub = "abcd";
					var priv = "abcd";
					return {
						alg: alg,
						extractable: extractable,
						keyUsages: keyUsages,
						publicKey: pub,
						privateKey: priv,
					}
				}
				throw "invalid key generation"
			},
			encrypt: function(alg, key, data) {
				if(alg.name == 'AES-CBC') {
				 	return [data, "iv"];
				} else {
					throw 'invalid algorithm'
				}
			},
			decrypt: function(alg, key, data) {
				if(alg.name == 'AES-CBC') {
					return data;
				} else {
					throw 'invalid algorithm'
				}
			},
			sign: function(alg, key, data){
				return data;
			},
			deriveKey: function(alg, masterKey, derivedKeyAlgorithm, extractable, keyUsages) {
				return {
					alg: derivedKeyAlgorithm,
					key: "abcd",
					keyUsages: keyUsages,
					extractable: extractable
				}
			},
			importKey: function(type, key, alg, extractable, keyUsages) {
				return {
					alg: alg,
					key: key,
					keyUsages: keyUsages,
					extractable: extractable
				}
			}
		}
	}
}

/*
 * net module mock
 */
var clientState = {
	init:false,
	connected:false,
	data:undefined,
	remoteAddress:'127.0.0.1',
	remotePort:'2222',
	connect: function(port, host, cb) {
		this.connected = true;
		serverState.clientConnected = true;
		return cb();
	},
	write: function(str, encoding){
		clientState.data = str;
	},
	on: function(type, cb) {
		if (type == 'close') {
			this.init = false;
			this.connected = false;
			return cb();
		} else if (type == 'data') {
			return cb(this.data);
		}
	},
	destroy: function() {
		serverState.clientConnected = false;
		this.init = false;
		this.connected = false;
	}
}

var serverState = {
	init:false,
	listening:false,
	clientConnected:false,
	msg:undefined,
	address:undefined,
	port:undefined,
	listen: function(port, host, cb) {
		this.listening = true;
		this.address = host;
		this.port = port;
		return cb();
	},
	on: function(type, cb) {
		if (type == 'connection') {
			clientState.connected = true;
			cb(clientState);
			return clientState;
		} else if (type == 'data') {
			return cb(this.data)
		}
	},
	write: function(str, encoding) {
		serverState.data = str
	},
	close: function() {
		this.listening =false;
		this.init = false;
	}
}

var internal = {
	createServer: function() {
		serverState.init = true;
		return serverState;
	},
}

function Socket() {
	clientState.init = true
	return clientState;
}

var net = {
	createServer: function() {
		return internal.createServer();
	},
	Socket: function() {
		clientState.init = true
		return clientState;
	}
}

/*
 * Mock for Uint8Array
 */
 function Uint8Array(size) {
 	var arr = [];
 	arr.length = size;
 	return arr;
 }

/*
 * Mock for command line arguments
 */
 var process = {
	argv: ["node", "mockedCryptoBoth.js", "Super Secret Message"]
};

/*WebCrypto Shim*/
SecAnn <!CSRV * Message * CryptKey * Signature!>;
SecAnn <!PrivKey * PubKey * SymKey!> Extends <!CryptKey!>;
SecAnn <!Plaintext * Ciphertext!> Extends <!Message!>;

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
var decShim = function(alg, key, data) {
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

/*
 * Client-Server Application Code
 */
//Server Prelims & Initialization
var host = '127.0.0.1';
var port = 3000;
var aliceStore = {
	server:true,
	name: "Alice",
	supported: "AES",
	keyExchAlg: "ECDH",
	generator: [5],
	prime: [23],
	keyPair: undefined,
	sharedSecret: undefined
}
var server = net.createServer();
var crypto = window.crypto;
derivePubPrivKeyPair(aliceStore);


//Client Prelims & Initialization
var client = new net.Socket();
var bobStore = {
	server:false,
	name: "Bob",
	keyPair: undefined,
	sharedSecret: undefined,
	generator:undefined,
	prime:undefined,
	keyExchAlg:undefined
}

//Start Listening
server.listen(port, host, function() {
	console.log("Server Listening: " + host + ":" + port);
});

//Client Connects
client.connect(port, host, function() {
	console.log("Connected to " + host + ":" + port + '.');
});

/*MODIFIED LOGIC TO MAKE SYNC*/
var soc = server.on('connection', function(socket) {
	onConnect(socket);
});

client.on('data', function(data) {
	onClientData(client, data)
});

soc.on('data', function(data) {
	onServerData(data, soc);
});

client.on('data', function(data) {
	onClientData(client, data)
});

soc.on('data', function(data) {
	onServerData(data, soc);
});

soc.on('close', function() {
	console.log("Client Closed: " + soc.remoteAddress + ":" + soc.remotePort);
});

client.on('close', function() {
	console.log('Connection Closed to ' + host + ":" + port + '.');
});
/*MODIFIED LOGIC TO MAKE SYNC*/

//On a Client Connecting, report the address and give out cryptoParams
function onConnect(socket) {
	console.log("Client Connected: " + socket.remoteAddress + ":" + socket.remotePort);
	var strPubKey = aliceStore.keyPair.publicKey.toString('utf8');
	var alg = 'name:ECDH,prime:' + aliceStore.prime + ',generator:' + aliceStore.generator + ',public:' + aliceStore.keyPair.publicKey + '';
	sendMsg(socket, "Hello, client. " + 
					"Supported Encryption:" + aliceStore.supported + '; KeyExchange:' + alg,
					aliceStore.name);
}

//Logic for handling incoming data to the Server
function onServerData(data, socket) {
	var str = data;
	console.log("Received from " + socket.remoteAddress + ":" + socket.remotePort + ": " + str);
	//check if message is a public key or a ciphertext
	var pubKeyGuard = /publicKey:(.*)/.exec(str);
	var ctGuard = /ct:(.*),iv:(.*)/.exec(str);
	if (pubKeyGuard != null) {
		var clientPubKey = importKey(aliceStore, pubKeyGuard[1]);
		computeSharedSecret(aliceStore, clientPubKey);
		sendMsg(socket, "ACK PubKey", aliceStore.name);
	} else if (ctGuard !=null) {
		var decrypted = decrypt(aliceStore, ctGuard[1], ctGuard[2]);
		if(decrypted != null) {
			console.log('Server has decrypted the message: ' + decrypted);
		} else {
			console.log('Server has failed to decrypt the message.');
		}
		finished(socket);
	} else {
		console.log('Server does not recognize the message.')		
		finished(socket);
	}
}

//Logic for handling incoming data to the Client
function onClientData(socket, data) {
	var str = data
	console.log('Received from ' + host + ':' + port + ': ' + str);
	if (/Supported Encryption:/.test(str)) {
		if(/none/.test(str)) {
			sendMsg(socket, getMsg, bobStore.name);
		} else if (/AES/.test(str)) {
			var theirPubKey = processParams(bobStore, str);
			derivePubPrivKeyPair(bobStore);
			sendMsg(socket, 
				"Hi, my public key is publicKey:" + bobStore.keyPair.publicKey + "",
				bobStore.name)
			computeSharedSecret(bobStore, theirPubKey);
		} else {
			throw 'No supplied encryption methods.'
		}
	} else if (/ACK/.test(str)) {
		var ct = encrypt(bobStore, getMsg());
		sendMsg(socket, ct, bobStore.name)
	}

}

//Auxilliary Functions
function finished(socket) {
	socket.destroy();
	server.close();
}

function importKey(store, pubKey) {
	return crypto.subtle.importKey("raw", pubKey, {name:store.keyExchAlg, prime:store.prime, generator:store.generator}, false, []);
}

function processParams(store, str) {
	var params = /KeyExchange:name:(.*),prime:(.*),generator:(.*),public:(.*)/.exec(str);
	if (params == null) { throw "invalid KeyExchange Parameters"}
	store.keyExchAlg = params[1];
	store.prime = parseInt(params[2]);
	store.generator = parseInt(params[3]);
	return importKey(store, params[4]);
}

function derivePubPrivKeyPair(store) {
	store.keyPair = crypto.subtle.generateKey({
		name: store.keyExchAlg,
		prime: store.prime,
		generator: store.generator
	}, false, ["deriveKey"]);
}

function computeSharedSecret(store, theirPubKey) {
	store.sharedSecret = crypto.subtle.deriveKey({
		name: store.keyExchAlg,
		prime: store.prime,
		generator: store.generator,
		public: theirPubKey
	},
	store.keyPair.privateKey,
	{name:"AES-CBC", length:128},
	false,
	["encrypt", "decrypt"]);
}

function getIV() {
	var iv = new Uint8Array(16);
	crypto.getRandomValues(iv);
	//Developer requires that IV should be encodable as ASCII characters
	//Commenting this out removes the security vulnerability
	//for (var i = 0; i < iv.length; i++) {
	//	iv[i] = iv[i] % 128;
	//}
	return iv;
}

function encrypt(store, str) {
	var ivEnc = getIV();
	var res = crypto.subtle.encrypt({
		name:'AES-CBC',
		length:128,
		iv: ivEnc,
	}, store.sharedSecret, str);
	return "{ct:"+res[0]+",iv:"+res[1]+"}"
}

function decrypt(store, str, iv) {
	return crypto.subtle.decrypt({
		name:'AES-CBC',
		length:128,
		iv: iv
	}, store.sharedSecret, str);
}

function sendMsg(socket, msg, usr) {
	var final = usr + " > " + msg;
	socket.write(final, 'utf8');
	console.log("Sent to " + socket.remoteAddress + ":" + socket.remotePort + ': ' + final);
}

function getMsg () {
	var args = process.argv;
	if (args.length != 3) {
		throw ("cryptoApp takes a single message as argument");
	} else {
		return args[2];
	}
}