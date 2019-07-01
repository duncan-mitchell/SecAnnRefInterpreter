/*
 * Client-Server Crypto Messaging Application.
 * The bug described in Section 5 of the paper is fixed.
 */
'use strict';
var crypto = require('./webcrypto-mock');
var net = require('net');

var host = '127.0.0.1';
var port = 3000;
var aliceStore = {
	server:true,
	name: "Alice",
	supported: "AES",
	keyExchAlg: "ECDH",
	generator: new Uint8Array([5]),
	prime: new Uint8Array([23]),
	keyPair: undefined,
	sharedSecret: undefined
}

//Initialize Server
var server = net.createServer();
derivePubPrivKeyPair(aliceStore);

//Start Listening
server.listen(port, host, function() {
	console.log("Server Listening: " + host + ":" + port);
});

//Main Server Logic
server.on('connection', function(socket) {
	onConnect(socket);
	socket.on('data', function(data) {
		onData(data, socket);
	});
	socket.on('close', function() {
		console.log("Client Closed: " + socket.remoteAddress + ":" + socket.remotePort);
	});
})

//Logic for handling incoming data
function onData(data, socket) {
	var str = data.toString('utf8')
	console.log("Received from " + socket.remoteAddress + ":" + socket.remotePort + ": " + str);
	
	//check if message is a public key or a ciphertext
	var pubKeyGuard = str.match(/{publicKey:(.*?)}/);
	var ctGuard = str.match(/{ct:(.*),iv:(.*)}/);
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

//On a Client Connecting, report the address and give out cryptoParams
function onConnect(socket) {
	console.log("Client Connected: " + socket.remoteAddress + ":" + socket.remotePort);
	var strPubKey = aliceStore.keyPair.publicKey.toString('utf8');
	var alg = '{name:ECDH,prime:' + aliceStore.prime + ',generator:' + aliceStore.generator + ',public:' + aliceStore.keyPair.publicKey + '}';
	sendMsg(socket, "Hello, client. " + 
					"Supported Encryption:" + aliceStore.supported + '; KeyExchange:' + alg,
					aliceStore.name);
}

//Client Code
var client = new net.Socket();
var host = '127.0.0.1'
var port = 3000;
var bobStore = {
	server:false,
	name: "Bob",
	keyPair: undefined,
	sharedSecret: undefined,
	generator:undefined,
	prime:undefined,
	keyExchAlg:undefined
}
client.connect(port, host, function() {
	console.log("Connected to " + host + ":" + port + '.');
});
client.on('close', function() {
	console.log('Connection Closed to ' + host + ":" + port + '.');
});

client.on('data', function(data) {
	var str = data.toString('utf8');
	console.log('Received from ' + host + ':' + port + ': ' + str);
	if (/Supported Encryption:/.test(str)) {
		if(/none/.test(str)) {
			sendMsg(client, getMsg, bobStore.name);
		} else if (/AES/.test(str)) {
			var theirPubKey = processParams(bobStore, str);
			derivePubPrivKeyPair(bobStore);
			sendMsg(client, 
				"Hi, my public key is {publicKey:" + bobStore.keyPair.publicKey + "}",
				bobStore.name)
			computeSharedSecret(bobStore, theirPubKey);
		} else {
			throw 'No supplied encryption methods.'
		}
	} else if (/ACK/.test(str)) {
		var ct = encrypt(bobStore, getMsg());
		sendMsg(client, ct, bobStore.name)
	}
});

//Auxilliary Functions
function finished(socket) {
	socket.destroy();
	server.close();
}

function importKey(store, pubKey) {
	return crypto.subtle.importKey("raw", pubKey, {name:store.keyExchAlg, prime:store.prime, generator:store.generator}, false, []);
}

function processParams(store, str) {
	var re = /KeyExchange:{name:(.*),prime:(.*),generator:(.*),public:(.*)}/;
	var params = str.match(re)
	if (params == null) { throw "invalid KeyExchange Parameters"}
	store.keyExchAlg = params[1];
	store.prime = new Uint8Array([parseInt(params[2])]);
	store.generator = new Uint8Array([parseInt(params[3])]);
	//return the imported public key
	return importKey(store, params[4]);
}

function derivePubPrivKeyPair(store) {
	store.keyPair = crypto.subtle.generateKey({
		name: store.keyExchAlg,
		prime: store.prime,
		generator: store.generator
	}, false, "deriveKey");
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