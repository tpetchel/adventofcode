// https://adventofcode.com/2021/day/16
open System

// --- Day 16: Packet Decoder ---

type Payload =
    | Literal of value : int
    | Subpackets of packets : Packet seq

and Packet = { 
    Version : int;
    TypeID : int;
    Payload : Payload }

let readInput filename = 
    (System.IO.File.ReadAllText filename).ToCharArray() |> seq

let hexToBinary (hex : string) =
    let table = Map [ 
        ('0', "0000");
        ('1', "0001");
        ('2', "0010");
        ('3', "0011");
        ('4', "0100");
        ('5', "0101");
        ('6', "0110");
        ('7', "0111");
        ('8', "1000");
        ('9', "1001");
        ('A', "1010");
        ('B', "1011");
        ('C', "1100");
        ('D', "1101");
        ('E', "1110");
        ('F', "1111"); ]
    let binary = hex |> Seq.fold (fun s h -> s + table[h]) ""
    binary |> seq

let binaryToDecimal (binary : char seq) = 
    match binary |> Seq.toArray with
    | [| '0' |] -> 0
    | [| '1' |] -> 1
    | _ ->
        let s = binary |> String.Concat
        System.Convert.ToInt32(s, 2);

// 1101 0010 1111 1110 0010 1000
// VVVT TTAA AAAB BBBB CCCC C

// 10111 11110 00101 000
// AAAAA BBBBB CCCCC

let countBy count = Seq.initInfinite (fun index -> index * count)

[<Literal>]
let LiteralChunkSize = 5

let rec decodePacket (binary : char seq) =

    let decodeLiteral version typeID binary = 
        printfn $"  Decoding literal..."
        let countByFives = countBy LiteralChunkSize
        let numChunks = 1 + (countByFives |> Seq.takeWhile (fun index -> binary |> Seq.item index <> '0') |> Seq.length)
        let chunks = binary |> Seq.take (numChunks * LiteralChunkSize) |> Seq.chunkBySize LiteralChunkSize
        // Concatenate last 4 bits of each chunk
        let literal = 
            chunks 
            |> Seq.fold (fun state s -> state + (s |> Seq.skip 1 |> String.Concat)) ""
            |> binaryToDecimal

        let packet = { Packet.Version = version; TypeID = typeID; Payload = Literal(literal); }
        (packet, 6 + (numChunks * LiteralChunkSize))

    let rec decodeOperator version typeID binary =
        printfn $"  Decoding operator..."
        let lengthTypeID = binary |> Seq.head
        printfn $"    Length type ID is {lengthTypeID}..."
        match lengthTypeID with
        | '0' -> 
            let lengthOfPackets = binaryToDecimal (binary |> Seq.skip 1 |> Seq.take 15)
            printfn $"    Length of packets is {lengthOfPackets}..."
            let mutable remainingBits = lengthOfPackets
            let mutable newBinary = binary |> Seq.skip 16 |> Seq.take lengthOfPackets
            let packets = seq {
                while remainingBits > 0 do
                    let (packet, bitsDecoded) = decodeSubpacket newBinary
                    remainingBits <- remainingBits - bitsDecoded
                    newBinary <- newBinary |> Seq.skip bitsDecoded
                    yield packet
            } 
            let packets' = packets |> Seq.toList
            let packet = { Packet.Version = version; TypeID = typeID; Payload = Subpackets(packets'); }
            (packet, 16 + lengthOfPackets)
        | '1' ->
            let numberOfPackets = binaryToDecimal (binary |> Seq.skip 1 |> Seq.take 11)
            printfn $"    Number of packets is {numberOfPackets}..."
            let mutable totalBitsDecoded = 0
            let mutable newBinary = binary |> Seq.skip 12
            let packets = seq {
                for _ in 1 .. numberOfPackets do 
                    let (packet, bitsDecoded) = decodeSubpacket newBinary
                    totalBitsDecoded <- totalBitsDecoded + bitsDecoded
                    newBinary <- newBinary |> Seq.skip bitsDecoded
                    yield packet
            }
            let packets' = packets |> Seq.toList
            let packet = { Packet.Version = version; TypeID = typeID; Payload = Subpackets(packets'); }
            (packet, 12 + totalBitsDecoded)
        | _ -> failwith $"Unexpected length type ID {lengthTypeID}"

    and decodeSubpacket binary =
        let version = binaryToDecimal (binary |> Seq.take 3)
        let typeID = binaryToDecimal (binary |> Seq.skip 3 |> Seq.take 3)
        printfn $"Found packet with version {version}; typeID {typeID}..."
        match typeID with
        | 4 -> decodeLiteral version typeID (binary |> Seq.skip 6)
        | _ -> decodeOperator version typeID (binary |> Seq.skip 6)

    let (p, bitsDecoded) = decodeSubpacket binary
    p

let rec printPacket indentLevel (packet : Packet) = 
    let tab = new String(' ', 2 * indentLevel)
    printfn $"{tab}Packet:"
    printfn $"{tab}  ver: {packet.Version}"
    printfn $"{tab}  tid: {packet.TypeID}"
    match packet.Payload with
    | Literal value -> printfn $"{tab}  lit: {value}"
    | Subpackets packets -> packets |> Seq.iter (fun p -> printPacket (indentLevel+1) p)

let rec sumVersions packet =
    let ver = packet.Version
    match packet.Payload with
    | Literal _ -> ver
    | Subpackets packets -> ver + (Seq.sumBy sumVersions packets)

// --- Part One ---

// Sample data
// let sampleLiteralPacket = decodePacket (hexToBinary "D2FE28")
// printPacket 1 sampleLiteralPacket

// printfn "==="

// let sampleOperatorPacket1 = decodePacket (hexToBinary "38006F45291200")
// printPacket 1 sampleOperatorPacket1

// printfn "==="

// let sampleOperatorPacket2 = decodePacket (hexToBinary "EE00D40C823060")
// printPacket 1 sampleOperatorPacket2

// printfn "==="

let sums = seq { "8A004A801A8002F478";
      "620080001611562C8802118E34";
      "C0015000016115A2E0802F182340";
      "A0016C880162017C3686B18A3D4780"; } |> Seq.skip 1 |> Seq.take 1 |> Seq.map (fun hex ->
    let packet = decodePacket (hexToBinary hex)
    sumVersions packet)
printfn "%A" sums
//printfn $"Sum = {sum}")

// --- Part Two ---


// TODO: Get clear on bitsProcessed, what to do with it, and padding