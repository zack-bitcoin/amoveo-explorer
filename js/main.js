(function main() {
    console.log("hello world.");
    //This contract is valid as of block 64255
    //eventually it will become invalid, so if you want to run the test you need to make a different contract to replcae this one.
    /*
      If this test succeeds, the javascript console shows this:

[-6,["v","6shH4FO3E3mZ7gBKwWv71NxT0FvUTqaVjhZ7ygMnfCI=",20000]]
main.js:20 ["oracle","6shH4FO3E3mZ7gBKwWv71NxT0FvUTqaVjhZ7ygMnfCI=",[-6],[-6,"Xy9Tecb4Xx88W4D+NW2CQgYrDIM+9m3r7d/zy6YNe7o="],"UCA9IHRoZSBwcmljZSBvZiBVU0QgaW4gVkVPIG9uIHF0cmFkZS5pbyBmcm9tIDAgdG8gMC4wNSBvbiBNYXkgMzEsIDIwMTksIGF0IDEyOjAwIE5vb24gR01UOyByZXR1cm4gUCAqIDEwMjQgLyAwLjA1IGJpdCBudW1iZXIgMA==",69420]
main.js:23 [-6,["channel_offer","Xy9Tecb4Xx88W4D+NW2CQgYrDIM+9m3r7d/zy6YNe7o=","6shH4FO3E3mZ7gBKwWv71NxT0FvUTqaVjhZ7ygMnfCI=",614,2,64339,10,10000,10000,2,"BBEuaxBNwXiTpEMTZI2gExMGpxCwAapTyFrgWMu5n4cIcqPojDz40Trf7xdWDlHL8KH+AvrTc2dhSC+35eSjmaQ="]]
main.js:26 [-6,[-6,2,3000,5000,"BBEuaxBNwXiTpEMTZI2gExMGpxCwAapTyFrgWMu5n4cIcqPojDz40Trf7xdWDlHL8KH+AvrTc2dhSC+35eSjmaQ=",0,1000000,10000,10000,"6shH4FO3E3mZ7gBKwWv71NxT0FvUTqaVjhZ7ygMnfCI=",64239,1000,[-7,2,"MEQCIEvjwRnANgJrhLfKiPyd3YHSvFXL7XA098Acw9fXrS46AiByuwStQoVjBetI2+GNhmCHA569JSjSxqoAhhAoL+ZRgg=="],"AAD67xOHJw/qyEfgU7cTeZnuAErBa/vU3FPQW9ROppWOFnvKAyd8IjBFAiEA8Y2dZkonQU4QXfm6LZqK3les3GP3HlkXRXoJxbiIDY0CIEVn/yOB7CazFCHLeFGUjhk3XkTUVsWYQFkw4Pz2xCPy",1,2,"Xy9Tecb4Xx88W4D+NW2CQgYrDIM+9m3r7d/zy6YNe7o=",10,818,0,0],["signed",["nc_offer","BBEuaxBNwXiTpEMTZI2gExMGpxCwAapTyFrgWMu5n4cIcqPojDz40Trf7xdWDlHL8KH+AvrTc2dhSC+35eSjmaQ=",10,64339,10000,10000,1000,1000,"Xy9Tecb4Xx88W4D+NW2CQgYrDIM+9m3r7d/zy6YNe7o=","nPwN6mIWS4JUIo2neplltEEifucqc43ytORXToDFtco="],"MEUCIQCwxOaubh3Y7yuPBWZUKJy1jnhqYhLy+U1vRLZNO/pU1AIgX81qJ8HVp0r/Ac48tqG6F7yyYC7gKhcEka4qVk18G9U=",[-6]]]

*/
    var contract = [-6,[-6,2,3000,5000,"BBEuaxBNwXiTpEMTZI2gExMGpxCwAapTyFrgWMu5n4cIcqPojDz40Trf7xdWDlHL8KH+AvrTc2dhSC+35eSjmaQ=",0,1000000,10000,10000,"6shH4FO3E3mZ7gBKwWv71NxT0FvUTqaVjhZ7ygMnfCI=",64239,1000,[-7,2,"MEQCIEvjwRnANgJrhLfKiPyd3YHSvFXL7XA098Acw9fXrS46AiByuwStQoVjBetI2+GNhmCHA569JSjSxqoAhhAoL+ZRgg=="],"AAD67xOHJw/qyEfgU7cTeZnuAErBa/vU3FPQW9ROppWOFnvKAyd8IjBFAiEA8Y2dZkonQU4QXfm6LZqK3les3GP3HlkXRXoJxbiIDY0CIEVn/yOB7CazFCHLeFGUjhk3XkTUVsWYQFkw4Pz2xCPy",1,2,"Xy9Tecb4Xx88W4D+NW2CQgYrDIM+9m3r7d/zy6YNe7o=",10,818,0,0],["signed",["nc_offer","BBEuaxBNwXiTpEMTZI2gExMGpxCwAapTyFrgWMu5n4cIcqPojDz40Trf7xdWDlHL8KH+AvrTc2dhSC+35eSjmaQ=",10,64339,10000,10000,1000,1000,"Xy9Tecb4Xx88W4D+NW2CQgYrDIM+9m3r7d/zy6YNe7o=","nPwN6mIWS4JUIo2neplltEEifucqc43ytORXToDFtco="],"MEUCIQCwxOaubh3Y7yuPBWZUKJy1jnhqYhLy+U1vRLZNO/pU1AIgX81qJ8HVp0r/Ac48tqG6F7yyYC7gKhcEka4qVk18G9U=",[-6]]];
    variable_public_get(["add", contract], function(X) {
        variable_public_get(["oracle_list"], function(X) {
            console.log(JSON.stringify(X));
            var OID = X[1][1];
            variable_public_get(["oracle", OID], function(Oracle) {
                console.log(JSON.stringify(Oracle[1]));
                var CID = Oracle[1][3][1];
                variable_public_get(["get_offers", [-6, CID]], function(Oracle) {
                    console.log(JSON.stringify(Oracle));
                });
                variable_public_get(["get_offer_contract", CID], function(C) {
                    console.log(JSON.stringify(C[1]));
                });
            });
        });
    });
})();
