import sys,asyncio,aiohttp,time,json

def isInt(value):
    try:
        int(value)
        return True
    except ValueError:
        return False

def isFloat(value):
    try:
        float(value)
        return True
    except ValueError:
        return False

def isLongitude(value):
    split1 = value.split('+')
    if len(split1) == 1: #no plus signs
        split3 = split1[0].split('-')
        if len(split3) != 3:
            return False
        split2 = ["","","",""]
        split2[0] = split3[0]
        split2[1] = split3[1]
        split2[3] = split3[2]
    elif len(split1) == 2: #one plus sign
        if len(split1[0]) > 0:
            split2 = split1[0].split('-')
            split3 = split1[1].split('-')
            split2.extend(split3)
        else:
            split3 = split1[1].split('-')
            if len(split3) != 2:
                return False
            split2 = ["",split3[0],"",split3[1]]
    elif len(split1) == 3: #two plus signs
        for i in split1:
            if len(i.split('-')) != 1:
                return False
        split2 = ["","","",""]
        split2[0] = split1[0]
        split2[1] = split1[1]
        split2[3] = split1[2]
    else:#three or more plus signs
        return False
    if len(split2) != 4 or len(split2[0]) != 0 or len(split2[2]) != 0:
        return False
    else:
        if isFloat(split2[1]) and isFloat(split2[3]):
            return True
        else:
            return False

def getLongitude(value):
    i = 1
    while i < len(value):
        if value[i] == '-' or value[i] == '+':
            break
        i = i+1
    return [float(value[:i]),float(value[i:])]

class Server:
    def __init__(self, name, port, ip='127.0.0.1', message_max_length=1e6):
        self.name = name
        self.ip = ip
        self.port = port
        self.message_max_length = int(message_max_length)
        self.clients = {}

    async def pass_message(self, string, num):
        #Hill: 12200
        #Jaquez: 12201
        #Smith: 12202
        #Campbell: 12203
        #Singleton: 12204
        if self.name == "Hill":
            ports = [12201,12202] #jaquez,smith
            if num == 0:
                output = "SERVER0 "+string
            else:
                output = "SERVER"+str(num)+" "+" ".join(string)
        elif self.name == "Jaquez":
            ports = [12200,12204] #hill,singleton
            if num == 0:
                output = "SERVER0 "+string
            else:
                output = "SERVER"+str(num)+" "+" ".join(string)
        elif self.name == "Smith":
            ports = [12200,12203,12204] #hill,singleton,campbell
            if num == 0:
                output = "SERVER0 "+string
            else:
                output = "SERVER"+str(num)+" "+" ".join(string)
        elif self.name == "Campbell":
            ports = [12202,12204] #smith,singleton
            if num == 0:
                output = "SERVER0 "+string
            else:
                output = "SERVER"+str(num)+" "+" ".join(string)
        elif self.name == "Singleton":
            ports = [12201,12202,12203] #jaquez,smith,campbell
            if num == 0:
                output = "SERVER0 "+string
            else:
                output = "SERVER"+str(num)+" "+" ".join(string)
        else:
            return #invalid name
        for port in ports:
            try:
                print("Attempting to connect to",port)
                #loop = asyncio.get_event_loop()
                reader, writer = await asyncio.open_connection(self.ip, port)#, loop=loop)
                print("Connected to",port,". Writing data.")
                writer.write(output.encode())
                await writer.drain()
                print("Closing connection to",port)
                writer.close()
                await writer.wait_closed()
                print("Connection to",port,"closed")
            except ConnectionRefusedError:
                print("Port",port,"is not open.")
            except ConnectionResetError:
                print("Connection to port",port,"closed prematurely.")
        return

    async def messageParser(self, reader, writer):
        client = writer.get_extra_info('peername')
        print("New connection to",client)
        try:
            while True:
                message = await reader.read(self.message_max_length)
                decoded = message.decode()
                if len(decoded) == 0:
                    print("Connection closed with",client)
                    return
                print(decoded)
                splitmessage = decoded.split()
                if len(splitmessage) == 4 and splitmessage[0] == "IAMAT" and isLongitude(splitmessage[2]) and isFloat(splitmessage[3]):
                    difference = time.time()-float(splitmessage[3])
                    print("Client",client,"is",splitmessage[1])
                    if difference > 0:
                        strtime = "+"+str(difference)
                    else:
                        strtime = str(difference)
                    output = "AT "+self.name+" "+strtime+" "+splitmessage[1]+" "+splitmessage[2]+" "+splitmessage[3]
                    #write output to other servers too
                    await self.pass_message(output,0)
                    
                    self.clients[splitmessage[1]] = output
                    writer.write(output.encode())
                    print(output)
                    await writer.drain()
                elif len(splitmessage) == 4 and splitmessage[0] == "WHATSAT" and isInt(splitmessage[2]) and isInt(splitmessage[3]):
                    if splitmessage[1] in self.clients and int(splitmessage[2]) < 50 and int(splitmessage[3]) < 20:
                        #need to add actual functionality (http request)
                        session = aiohttp.ClientSession()
                        longitude = getLongitude(self.clients[splitmessage[1]].split(' ')[4])
                        location = "location="+str(longitude[0])+","+str(longitude[1])
                        radius = "&radius="+str(int(splitmessage[2])*1000)
                        key = "&key=AIzaSyCL_8paVGeMOByRa0Tcy6dZJysGLN4ptTA"
                        url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?'+location+radius+key
                        print("Requesting from:",url)
                        response = await session.get(url)
                        print("{0} status: {1}".format(url, response.status))
                        responsetext = await response.text()
                        print("{0} responded: {1}".format(url, responsetext))
                        await session.close()

                        outputjson = json.loads(responsetext)
                        slicedresults = outputjson["results"][:int(splitmessage[3])]
                        outputjson["results"] = slicedresults
                        modifiedresponse = json.dumps(outputjson, indent=4)
                        output = self.clients[splitmessage[1]]+'\n'+modifiedresponse+'\n\n'
                        writer.write(output.encode())
                        print(output)
                        await writer.drain()
                    else:
                        output = "ERROR Invalid WHATSAT Parameters: "+decoded
                        writer.write(output.encode())
                        print(output)
                        await writer.drain()
                elif len(splitmessage) == 7 and splitmessage[0] == "SERVER0" and splitmessage[1] == "AT" and isFloat(splitmessage[3]) and isLongitude(splitmessage[5]) and isFloat(splitmessage[6]):
                    #SERVER AT NAME TIME CLIENT COORDINATES TIMESTAMP
                    if self.name != splitmessage[2] and (splitmessage[2] == "Hill" or splitmessage[2] == "Jaquez" or splitmessage[2] == "Smith" or splitmessage[2] == "Campbell" or splitmessage[2] == "Singleton"):
                        splitmessage.pop(0)
                        self.clients[splitmessage[3]] = " ".join(splitmessage)
                        await self.pass_message(splitmessage,1)
                    elif self.name == splitmessage[2]:
                        print("Received own message")
                        continue
                    else:
                        print("Invalid server name:",splitmessage[2])
                elif len(splitmessage) == 7 and splitmessage[0] == "SERVER1" and splitmessage[1] == "AT" and isFloat(splitmessage[3]) and isLongitude(splitmessage[5]) and isFloat(splitmessage[6]):
                    #SERVER AT NAME TIME CLIENT COORDINATES TIMESTAMP
                    if splitmessage[2] != self.name and (splitmessage[2] == "Hill" or splitmessage[2] == "Jaquez" or splitmessage[2] == "Smith" or splitmessage[2] == "Campbell" or splitmessage[2] == "Singleton"):
                        splitmessage.pop(0)
                        self.clients[splitmessage[3]] = " ".join(splitmessage)
                        await self.pass_message(splitmessage,2)
                    elif self.name == splitmessage[2]:
                        print("Received own message")
                        continue
                    else:
                        print("Invalid server name:",splitmessage[2])
                elif len(splitmessage) == 7 and splitmessage[0] == "SERVER2" and splitmessage[1] == "AT" and isFloat(splitmessage[3]) and isLongitude(splitmessage[5]) and isFloat(splitmessage[6]):
                    #SERVER AT NAME TIME CLIENT COORDINATES TIMESTAMP
                    if splitmessage[2] != self.name and (splitmessage[2] == "Hill" or splitmessage[2] == "Jaquez" or splitmessage[2] == "Smith" or splitmessage[2] == "Campbell" or splitmessage[2] == "Singleton"):
                        splitmessage.pop(0)
                        self.clients[splitmessage[3]] = " ".join(splitmessage)
                    elif self.name == splitmessage[2]:
                        print("Received own message")
                        continue
                    else:
                        print("Invalid server name:",splitmessage[2])
                else:
                    output = "? "+decoded
                    writer.write(output.encode())
                    print(output)
                    await writer.drain()
        except ConnectionResetError as error:
            print("Connection Reset Error with client {0}: {1}".format(client, error))
        except KeyboardInterrupt:
            raise KeyboardInterrupt
        except:
            print("Other Exception:", sys.exc_info()[0], sys.exc_info()[1])

    def run_until_interrupted(self):
        loop = asyncio.get_event_loop()
        coro = asyncio.start_server(self.messageParser, self.ip, self.port, loop=loop)
        server = loop.run_until_complete(coro)

        try:
            loop.run_forever()
        except KeyboardInterrupt:
            pass
        # Close the server
        server.close()
        loop.run_until_complete(server.wait_closed())
        loop.close()

if __name__ == "__main__":
    num = len(sys.argv)
    if num != 2:
        print("Invalid number of args:", num)
        exit(1)
    name = sys.argv[1]
    if name == "Hill":
        port = 12200
    elif name == "Jaquez":
        port = 12201
    elif name == "Smith":
        port = 12202
    elif name == "Campbell":
        port = 12203
    elif name == "Singleton":
        port = 12204
    else:
        print("Invalid Name:",name)
        exit(1)
    sys.stdout = open(name+"log", 'a')
    server = Server(name,port)
    server.run_until_interrupted()
