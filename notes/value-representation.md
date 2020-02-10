| type           | mask | tag3 | what        |
|----------------|------|------|-------------|
| fixnum         | 0x07 | 0x00 | `000` |
| pair           | 0x07 | 0x01 | `001` |
| flonum         | 0x07 | 0x02 | `010` |
| symbol         | 0x07 | 0x03 | `011` |
| procedure      | 0x07 | 0x05 | `101` |
| typed object   | 0x07 | 0x07 | `111` |
| boolean        | 0xFF | 0x06 | `0000 0110` |
| false          | -    | 0x06 | `0000 0110` |
| true           | -    | 0x0E | `0000 1110` |
| char           | 0xFF | 0x16 | `0001 0110` |
| unbound        | 0xFF | 0x1E | `0001 1110` |
| nil            | 0xFF | 0x26 | `0010 0110` |
| forward marker | 0xFF | 0x2E | `0010 1110` |
| eof            | 0xFF | 0x36 | `0011 0110` |
| void           | 0xFF | 0x3E | `0011 1110` |
| vector         | 0    | 0    | `000` |
| *skip*         |      |      | `001` |
| string         | 0x07 | 0x02 | `010` |
| record         | 0x07 | 0x03 | `111` |

