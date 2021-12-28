// https://adventofcode.com/2021/day/16
open System

// --- Day 16: Packet Decoder ---

type Payload =
    | Literal of value : uint64
    | Subpackets of packets : Packet seq

and Packet = { 
    Version : int;
    TypeID : int;
    Payload : Payload }

let readInput filename = System.IO.File.ReadAllText filename

let debugPrint s =
    //printfn "%s" s
    s |> ignore

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
    let s = binary |> String.Concat
    System.Convert.ToInt32(s, 2);

let binaryToLargeDecimal (binary : char seq) = 
    let s = binary |> String.Concat
    System.Convert.ToUInt64(s, 2);

let countBy count = Seq.initInfinite (fun index -> index * count)

[<Literal>]
let LiteralChunkSize = 5

let rec decodePacket (binary : char seq) =

    let decodeLiteral version typeID binary = 
        debugPrint $"  Decoding literal..."
        let countByFives = countBy LiteralChunkSize
        let numChunks = 1 + (countByFives |> Seq.takeWhile (fun index -> binary |> Seq.item index <> '0') |> Seq.length)
        let chunks = binary |> Seq.take (numChunks * LiteralChunkSize) |> Seq.chunkBySize LiteralChunkSize
        // Concatenate last 4 bits of each chunk
        let literal = 
            chunks 
            |> Seq.fold (fun state s -> state + (s |> Seq.skip 1 |> String.Concat)) ""
            |> binaryToLargeDecimal

        debugPrint $"    Value = {literal}"
        let packet = { Packet.Version = version; TypeID = typeID; Payload = Literal(literal); }
        (packet, binary |> Seq.skip (numChunks * LiteralChunkSize))

    let rec decodeOperator version typeID binary =
        debugPrint $"  Decoding operator..."
        let lengthTypeID = binary |> Seq.head
        debugPrint $"    Length type ID is {lengthTypeID}..."
        match lengthTypeID with
        | '0' -> 
            let lengthOfPackets = binaryToDecimal (binary |> Seq.skip 1 |> Seq.take 15)
            debugPrint $"    Length of packets is {lengthOfPackets}..."
            // TODO: Use a fold to make immutable?
            let mutable newBinary = binary |> Seq.skip 16 |> Seq.take lengthOfPackets
            let packets = seq {
                while not (newBinary |> Seq.isEmpty) do
                    let (packet, newBinary') = decodeSubpacket newBinary
                    newBinary <- newBinary'
                    yield packet
            } 
            // TODO: Use toList to eagerly evaluate. Not idiomatic.
            let packets' = packets |> Seq.toList
            let packet = { Packet.Version = version; TypeID = typeID; Payload = Subpackets(packets'); }
            (packet, binary |> Seq.skip (16 + lengthOfPackets))
        | '1' ->
            let numberOfPackets = binaryToDecimal (binary |> Seq.skip 1 |> Seq.take 11)
            debugPrint $"    Number of packets is {numberOfPackets}..."
            // TODO: Use a fold to make immutable?
            let mutable newBinary = binary |> Seq.skip 12
            let packets = seq {
                for _ in 1 .. numberOfPackets do 
                    let (packet, newBinary') = decodeSubpacket newBinary
                    newBinary <- newBinary'
                    yield packet
            }
            // TODO: Use toList to eagerly evaluate. Not idiomatic.
            let packets' = packets |> Seq.toList
            let packet = { Packet.Version = version; TypeID = typeID; Payload = Subpackets(packets'); }
            (packet, newBinary)
        | _ -> failwith $"Unexpected length type ID {lengthTypeID}"

    and decodeSubpacket binary =
        let version = binaryToDecimal (binary |> Seq.take 3)
        let typeID = binaryToDecimal (binary |> Seq.skip 3 |> Seq.take 3)
        debugPrint $"Found packet with version {version}; typeID {typeID}..."
        match typeID with
        | 4 -> decodeLiteral version typeID (binary |> Seq.skip 6)
        | _ -> decodeOperator version typeID (binary |> Seq.skip 6)

    let (p, _) = decodeSubpacket binary
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
let sampleLiteralPacket = decodePacket (hexToBinary "D2FE28")
printPacket 1 sampleLiteralPacket

printfn "==="

let sampleOperatorPacket1 = decodePacket (hexToBinary "38006F45291200")
printPacket 1 sampleOperatorPacket1

printfn "==="

let sampleOperatorPacket2 = decodePacket (hexToBinary "EE00D40C823060")
printPacket 1 sampleOperatorPacket2

printfn "==="

let moreSamples = seq {
    "8A004A801A8002F478";
    "620080001611562C8802118E34";
    "C0015000016115A2E0802F182340";
    "A0016C880162017C3686B18A3D4780"; }
let sums = moreSamples |> Seq.map (fun hex ->
    let packet = decodePacket (hexToBinary hex)
    sumVersions packet)
printfn "%A" sums

// Puzzle data
let input = readInput "input.txt"
let packet = decodePacket (hexToBinary input)
printfn "%i" (sumVersions packet)

// --- Part Two ---

let rec evaluate packet =
    match packet.Payload with
    | Literal literal -> literal
    | Subpackets packets -> 
        let values = packets |> Seq.map (fun packet -> evaluate packet)
        match packet.TypeID with
        | 0 -> values |> Seq.sum
        | 1 -> values |> Seq.fold (fun state v -> state * v) 1UL
        | 2 -> values |> Seq.min
        | 3 -> values |> Seq.max
        | 5 ->
            let a = values |> Seq.head
            let b = values |> Seq.last
            if a > b then 1UL
            else 0UL
        | 6 ->
            let a = values |> Seq.head
            let b = values |> Seq.last
            if a < b then 1UL
            else 0UL
        | 7 ->
            let a = values |> Seq.head
            let b = values |> Seq.last
            if a = b then 1UL
            else 0UL
        | _ -> failwith $"Unexpected type ID {packet.TypeID}"

let evenMoreSamples = seq {
    "C200B40A82";
    "04005AC33890";
    "880086C3E88112";
    "CE00C43D881120";
    "D8005AC2A8F0";
    "F600BC2D8F";
    "9C005AC2F8F0";
    "9C0141080250320F1802104A08"; }
evenMoreSamples |> Seq.iter (fun hex ->
    let packet = decodePacket (hexToBinary hex)
    let result = evaluate packet
    printfn "%u" result)

// Puzzle data
printfn "%u" (evaluate packet)
