pragma circom 2.1.2;

include "../circomlib/circuits/poseidon.circom";
include "../circomlib/circuits/comparators.circom";
include "../circomlib/circuits/multiplexer.circom";
include "../circomlib/circuits/escalarmulany.circom";
include "../hashing_circuits/sha2/sha512/sha512_hash_bits.circom";

template CardanoWBPS(n, sb_n, sb_o) {
    var bits = 254;
    assert(n % bits == 0);
    var cmsg_elem = n\bits;
    
    // public inputs
    signal input A[256];
    signal input R8[256];
    signal input ek[2]; // public encryption key: msg -> Cmsg
    signal input C0[2]; 
    signal input Cmsg[cmsg_elem];
    signal input c_schnorr[64];
    signal input msg_pub[n];

    // private witness inputs
    signal input rho;
    signal input msg_priv[sb_n];

    var i;
    var j;

    var base[2] = [5299619240641551281634865583518297030282874472190772894086521144482721001553,
            16950150798460657717958625567821834550301663161624707787222815936182638968203];   

    // msg constructed from pub an priv parts
    signal msg[n];
    for(i=0; i<n; i++) {
        if(i >= sb_o && i < sb_o+sb_n) {
            msg[i] <== msg_priv[i - sb_o];
        } else {
            msg[i] <== msg_pub[i];
        }
    }

    //ElGamal Enc
    // H(h^rho1) + msg == c1
    component n2b_rho1= Num2Bits(251);  //a field element has 254 bit, but we only care about the 251 first bits. Revisit this point and rethink
    component ek_rho1 = EscalarMulAny(251);
    ek_rho1.p[0] <== ek[0];
    ek_rho1.p[1] <== ek[1];

    rho ==> n2b_rho1.in;
    for  (i=0; i < 251; i++) {
        n2b_rho1.out[i] ==> ek_rho1.e[i];
    }

    // C_0 == g^rho
    component g_pow_rho = EscalarMulFix(251, base);    
    for  (i=0; i < 251; i++) {
        n2b_rho1.out[i] ==> g_pow_rho.e[i];
    }
    log(g_pow_rho.out[0]);
    log(g_pow_rho.out[1]);
    C0[0] ===  g_pow_rho.out[0];
    C0[1] ===  g_pow_rho.out[1];


    component pEx = PoseidonEx(2, 1);
    pEx.initialState <== 0;  //Why setting it to 0.. need to read the paper
    pEx.inputs[0] <== ek_rho1.out[0];
    pEx.inputs[1] <== ek_rho1.out[1];



    var rand[cmsg_elem+1];
    rand[0] = pEx.out[0];
    //this is a bit of a mess.. seems we cant squeeze the spong arbitrary many times so this served as a quick workaround. I use PoseidonEx(1,2) since its more efficient that 1-1 poseidon it seems..
    component randEnc[cmsg_elem];
    for(var i=1;i<cmsg_elem;i+=2){
        randEnc[i] = PoseidonEx(1, 2);
        randEnc[i].initialState <== 0;
        randEnc[i].inputs[0] <== rand[i-1]; 
        rand[i] = randEnc[i].out[0];
        rand[i+1] = randEnc[i].out[1];
    }
    component msgBits2Num[cmsg_elem];
    for(var i=0;i<cmsg_elem;i++){
        msgBits2Num[i] = Bits2Num(bits);
        for  (var j=0; j < bits; j++) {
            msgBits2Num[i].in[j] <== msg[i*bits+j];
        }
        log(msgBits2Num[i].out + rand[i]);
        Cmsg[i] === msgBits2Num[i].out + rand[i] ;
    }

    // compute SHA-512 hash in EdDSA and check challenge
    component hash = Sha512_hash_bits_digest(n+256+256);
    for (i=0; i<256; i+=8) {
        for(j=0; j<8; j++) {
            hash.inp_bits[i+j] <== R8[i+(7-j)];
            hash.inp_bits[256+i+j] <== A[i+(7-j)];
        }
    }
    for (i=0; i<n; i++) {
        msg[i] ==> hash.inp_bits[512+i];
    }


    for(i=0; i<64; i++) {
        log(hash.hash_bytes[i]);
        c_schnorr[i] === hash.hash_bytes[i];
    }
}

component main {public [A,R8,ek,C0,Cmsg,c_schnorr,msg_pub]}= CardanoWBPS(9*254, 333, 32);
