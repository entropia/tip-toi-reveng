import sys
import os
import xml.etree.ElementTree
import re
import json
import hashlib
try:
    import boto3
except:
    print('Could not import boto3. Please install it using "pip3 install boto3"')

if len(sys.argv)!=2:
    print("Usage:   python3 polly.py [input file]")
xmlFile = sys.argv[1]

with open(xmlFile, "r") as f:
    xmlRaw = "<wrapper xmlns:amazon=\"http://www.amazon.com\">\n"+f.read()+"\n</wrapper>"

try:
    with open(xmlFile[:-4]+".pollycache.json", "r") as f:
        fileCache = json.loads(f.read())
except:
    fileCache = {}
    print("Could not open file cache, recreating all files over Polly.")

if not "files" in fileCache:
    fileCache["files"] = {}


def startPolly(awsAccessKeyId, awsSecretAccessKey):
    pollyClient = boto3.Session(
                        aws_access_key_id=awsAccessKeyId,
                        aws_secret_access_key=awsSecretAccessKey,
                        region_name="eu-central-1").client("polly")
    try:
        response = pollyClient.synthesize_speech(VoiceId="Joanna",OutputFormat="ogg_vorbis",Text="")
    except:
        print("Error: Invalid credentials!")
        raise
        return False
    return pollyClient

def saveFileCache():
    global xmlFile
    with open(xmlFile[:-4]+".pollycache.json", "w") as f:
        f.write(json.dumps(fileCache))

if "awsAccessKeyId" not in fileCache or "awsSecretAccessKey" not in fileCache:
    awsAccessKeyId     = input("Amazon AWS Access Key ID:     ")
    awsSecretAccessKey = input("Amazon AWS Access Secret Key: ")
    print("Testing access")
    pollyClient = startPolly(awsAccessKeyId, awsSecretAccessKey)
    if not pollyClient:
        exit(1)
    print("Credentials are working.")
    if (input("Do you want me to save those credentials? [y/N] ").lower() == "y"):
        fileCache["awsAccessKeyId"]     = awsAccessKeyId
        fileCache["awsSecretAccessKey"] = awsSecretAccessKey
        print("Saving credentials")
        saveFileCache()
    else:
        print("Not saving credentials")
else:
    print("Found stored AWS credentials")
    print("Testing access")
    pollyClient = startPolly(fileCache["awsAccessKeyId"], fileCache["awsSecretAccessKey"])
    if not pollyClient:
        print("Deleting stored credentials")
        del fileCache["awsAccessKeyId"]
        del fileCache["awsSecretAccessKey"]
        saveFileCache()
        print("Please restart the program.")
        exit(1)

xml.etree.ElementTree.register_namespace("amazon", "http://www.amazon.com")
root = xml.etree.ElementTree.fromstring(xmlRaw)

for polly in root:
    fileName = polly.attrib["file"]
    voice = polly.attrib["voice"]
    content = xml.etree.ElementTree.tostring(polly[0]).decode("utf-8").strip()
    content = re.sub("^<speak xmlns:amazon=\"http://www.amazon.com\">", "<speak>", content)
    contentMd5 = hashlib.md5(content.encode("utf-8")).hexdigest()
    
    if (contentMd5 != fileCache["files"].get(fileName, {}).get("contentMd5","")) or \
            (not os.path.exists(fileName)) or \
            (hashlib.md5(open(fileName,'rb').read()).hexdigest() != fileCache["files"].get(fileName, {}).get("fileMd5","")):
        
        print("Generating "+fileName)
        response = pollyClient.synthesize_speech(VoiceId=voice,OutputFormat="ogg_vorbis",TextType="ssml",Text=content)
        
        with open(fileName, "wb") as f:
            f.write(response['AudioStream'].read())
        
        fileCache["files"][fileName] = {
                "contentMd5": contentMd5,
                "fileMd5":    hashlib.md5(open(fileName,'rb').read()).hexdigest(),
            }
        saveFileCache()
    else:
        print("File "+fileName+" does not need to be generated.")
