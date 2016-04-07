/* Copyright (c) 2015, IBM 
 * Author(s): Dan Williams <djwillia@us.ibm.com> 
 *
 * Permission to use, copy, modify, and/or distribute this software
 * for any purpose with or without fee is hereby granted, provided
 * that the above copyright notice and this permission notice appear
 * in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
 * WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
 * AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include "solo5.h"

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

CAMLprim value stub_net_dbg(value arg) {
    CAMLparam1(arg);

    const char *str = String_val(arg);
    printf("%s: %s\n", __FILE__, str);

    CAMLreturn(Val_unit);
}

CAMLprim value stub_net_mac(value unit) {
    CAMLparam1(unit);
    printf("%s: WARNING: returning hardcoded MAC\n", __FILE__);
    CAMLreturn(caml_copy_string("52:54:00:12:34:56"));
}

CAMLprim value stub_net_read(value buffer, value num) {
    CAMLparam2(buffer, num);
    uint8_t *data = Caml_ba_data_val(buffer);
    int n = Int_val(num);
    int len;
    uint8_t *pkt;

    assert(Caml_ba_array_val(buffer)->num_dims == 1);
    

    pkt = virtio_net_pkt_get(&len);
    if ( !pkt )
        CAMLreturn(Val_int(-1));
    
    //printf("%s: got network pkt\n", __FILE__);

    if (0){
        int i;    
        for (i = 0; i < len; i++) {
            if ((i % 16) == 0) 
                printf("%04x:  ", i / 16);
            printf("%02x", pkt[i]);
            if ((i % 2) == 1)
                printf(" ");
            if ((i % 16) == 15)
                printf("\n");
        }
        printf("\n");
    }

    assert(len <= n);

    /* also, it's clearly not zero copy */
    memcpy(data, pkt, len);

    virtio_net_pkt_put();

    CAMLreturn(Val_int(len));
}

CAMLprim value stub_net_write(value buffer, value num) {
    CAMLparam2(buffer, num);
    uint8_t *data = Caml_ba_data_val(buffer);
    int n = Int_val(num);
    int ret;

    assert(Caml_ba_array_val(buffer)->num_dims == 1);
    
    //printf("%s: sending network pkt\n", __FILE__);
    ret = virtio_net_xmit_packet(data, n) ? -1 : n;
    //printf("%s: returning %d\n", __FILE__, ret);

    CAMLreturn(Val_int(ret));
}
