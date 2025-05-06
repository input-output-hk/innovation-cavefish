
//code by Mottla so dont trust it. Not sufficiently testet!!
//secp256k1 curve and big int implementation 0xPARC's https://github.com/0xPARC/circom-ecdsa
pragma circom 2.1.2;

include "../circomlib/circuits/poseidon.circom";
include "../circomlib/circuits/comparators.circom";
include "../circomlib/circuits/multiplexer.circom";
include "../circomlib/circuits/babyjub.circom";
include "../circomlib/circuits/escalarmulany.circom";
include "../circomlib/circuits/sha256/sha256.circom";


template Main(n,k) {
    signal input X[2][k];
    signal input R[2][k];
    signal input cc[k];
    signal input Cmsg;
    signal input ek[2];
    signal input C0[2]; 
    signal input pred;
    signal input msgToSign;
    signal input witness[8];
    signal input rho;
    var bitLength = n*k;
    //somehow they reserve 100 slots but only write into first k.. 
    
 
    var bits = 254; //the order r of the babyjub curve is 251 bits. 
    var i;
    var base[2] = [5299619240641551281634865583518297030282874472190772894086521144482721001553,
            16950150798460657717958625567821834550301663161624707787222815936182638968203]; 


    component hashWitness = Sha256(bits*8);
    component witnessBits[8]; 
    for  (var i=0; i<8; i++) {
        witnessBits[i] = Num2Bits(bits); 
        witness[i] ==> witnessBits[i].in;
        for  (var j=0; j < bits; j++) {
            witnessBits[i].out[j] ==> hashWitness.in[i*bits+j];
        }
    }
    
    component hashVal = Bits2Num(bits);    
    for  (var j=0; j < bits; j++) {
        hashVal.in[j] <== hashWitness.out[j];
    }    
    log("hash witness");
    log(hashVal.out);
    hashVal.out === msgToSign;            

    component tx = Bits2Num(16);    
    for  (var j=0; j < 16; j++) {
        tx.in[j] <== witnessBits[1].out[j];
    }    
    log("tx value");
    log(tx.out);
    log("pred value");
    log(pred);
    component leq = LessEqThan(16);
    leq.in[0] <== tx.out;
    leq.in[1] <== pred;
    log("comparing value < pred:");
    log(leq.out);
    1 === leq.out;
    //ElGamal Enc
    // h^rho1 * msg == c1
    component n2b_rho1= Num2Bits(251);  //a field element has 254 bit, but we only care about the 251 first bits. Revisit this point and rethink
    component ek_rho1 = EscalarMulAny(251);
    ek_rho1.p[0] <== ek[0];
    ek_rho1.p[1] <== ek[1];
    
    rho ==> n2b_rho1.in;
    for  (i=0; i < 251; i++) {
        n2b_rho1.out[i] ==> ek_rho1.e[i];
    }
        // C_0 == g^rho
    component g_pow_rho = EscalarMulFix(251,base);    
    for  (i=0; i < 251; i++) {
        n2b_rho1.out[i] ==> g_pow_rho.e[i];
    }
    log("comparing C0");
    log(g_pow_rho.out[0]);
    log(g_pow_rho.out[1]);
    C0[0] ===  g_pow_rho.out[0];
    C0[1] ===  g_pow_rho.out[1];

    
    //hash alpha and beta group elements to obtain uniform blinding factors
    component pEx = PoseidonEx(2, 3);
    pEx.initialState <== 0;  //Why setting it to 0.. need to read the paper
    pEx.inputs[0] <== ek_rho1.out[0];
    pEx.inputs[1] <== ek_rho1.out[1];  //we might not need to do this.. too little entropy anyway?
    

    log("hashed alpha");
    log(pEx.out[0]);
    log("hashed beta");
    log(pEx.out[1]);
    log("hashed message blinding");
    log(pEx.out[2]);    
    log("assert encryption of message is equal to the provided ciphertext");   
    log(msgToSign+pEx.out[2]);
 
    Cmsg === msgToSign+pEx.out[2]; 


    //prepare to create the Schnorr challenge c = H(R,X,m) using SHA256
    component hash = Sha256(bitLength*3);

    //write the x coordinate of R' into the hash input
    component R_x_bits[k];   
    for (var idx = 0; idx < k; idx++) {
        R_x_bits[idx] = Num2Bits(n);    
        R[0][idx] ==> R_x_bits[idx].in;
        for (var i=0; i<n; i++) {
            R_x_bits[idx].out[i] ==> hash.in[i+n*idx];
        }
    } 
    //Write the public key X's x-coordinate into the hash input
    component X_x_bits[k];   
    for (var idx = 0; idx < k; idx++) {
        X_x_bits[idx] = Num2Bits(n);    
        X[0][idx] ==> X_x_bits[idx].in;
        for (var i=0; i<n; i++) {
            X_x_bits[idx].out[i] ==> hash.in[bitLength+i+n*idx];
        }
    } 

    //write the message m into the hash input
    component msgbits = Num2Bits(bitLength);    
    msgToSign ==> msgbits.in;
    for  (var i=0; i<bitLength; i++) {
        msgbits.out[i] ==> hash.in[(bitLength*2)+i];
    }
 
    //split the hash output into 4x64 bit chuncks
    component fin[k];
    for (var idx = 0; idx < k; idx++) {
        fin[idx] = Bits2Num(n);
        for (var i=0; i<n; i++) {
            hash.out[i+n*idx] ==> fin[idx].in[i];
        }
    }
    
    for (var idx = 0; idx < k; idx++) {        
        cc[idx] === fin[idx].out;
    }       
}



component main {public [X,R,cc,pred,Cmsg,C0]}= Main(64,4);
