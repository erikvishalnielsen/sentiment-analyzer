import yt_dlp
import subprocess
import os
import whisper
import sys
import time
import asyncio
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
import glob
# import threading
# import time

# Configure paths and URLs
LIVE_STREAM_URL = 'https://www.youtube.com/watch?v=-mvUkiILTqI'  # Replace with the actual URL
TEMP_AUDIO_FILE = 'live_stream_audio'
PART_FILE = 'live_stream_audio.part'
PART_2 = 'live_stream_audio.wav.part'
TEMP_A2 = 'live_stream_audio.wav'
CONVERTED_AUDIO_FILE = 'converted_audio.wav'
GOOGLE_APPLICATION_CREDENTIALS = 'path/to/your/service_account_key.json'

def get_live_sentiment(content):
    analyzer = SentimentIntensityAnalyzer()

    content_scores = analyzer.polarity_scores(content)

    return content_scores.get("compound")

async def download_audio_part(url: str, output_file: str):
    directory = "/home/ubuntu/sentiment-analyzer/"
    files_to_delete = glob.glob(directory + "*.wav") + glob.glob(directory + "*.part")
    for file in files_to_delete:
        os.remove(file)
    
    part_number = 0

    for x in range(10):
        print("Downloading audio...", flush=True)
        # Ensure the output file has a .part extension
        part_file = f"{output_file}{part_number}.part"
        ydl_opts = {
            'format': 'bestaudio/best',
            'outtmpl': part_file,
            'noplaylist': True,
            'quiet': True,
            'username': 'oauth2',
            'password': '',
            # 'cookies-from-browser': 'chrome',

        }
        part_number += 1
    
        with yt_dlp.YoutubeDL(ydl_opts) as ydl:
            task = asyncio.get_running_loop().run_in_executor(None, ydl.download, [url])
            try:
                await asyncio.wait_for(task, timeout=10.0)
                
            except asyncio.TimeoutError:
                task.cancel()
                print("hi")

        print("Downloaded", flush=True)
        await asyncio.sleep(1)
        yield part_file
    


async def convert(output_file: str):
    def convert_part_to_wav(part_file):
        print("Converting audio pt1...", flush=True)
        output_file = TEMP_AUDIO_FILE
        part_file = part_file + ".part"
        # Ensure the output file has a .wav extension
        wav_file = f"{output_file}.wav"
        # Construct the ffmpeg command
        command = [
            'ffmpeg',
            '-i', part_file,
            '-vn',  # No video
            '-acodec', 'pcm_s16le',  # Audio codec
            '-ar', '44100',  # Audio sample rate
            '-ac', '2',  # Number of audio channels
            wav_file,
        ]
        # Run the ffmpeg command
        subprocess.run(command, check=True)
        os.remove(part_file)

    def convert_wav_to_wav():
        print("Converting audio pt2...", flush=True)
        input_file = TEMP_A2
        output_file = CONVERTED_AUDIO_FILE
        subprocess.run([
            'ffmpeg', '-i', input_file, 
            '-ar', '16000', '-ac', '1', '-f', 'wav', 
            output_file
        ], check=True)
        os.remove(TEMP_A2)

    # Function to transcribe audio using Google Cloud Speech-to-Text
    def transcribe_audio():
        print("Transcribing audio...", flush=True)
        file_path = CONVERTED_AUDIO_FILE
        model = whisper.load_model("base")  # Load Whisper model
        result = model.transcribe(file_path)  # Transcribe the audio file
        # json.dumps(result)
        print("Transcript: {}".format(result['text']), flush=True)
        print(get_live_sentiment(result['text']))
        os.remove(CONVERTED_AUDIO_FILE)
    
    async for part_file in download_audio_part(LIVE_STREAM_URL, TEMP_AUDIO_FILE):
        convert_part_to_wav(part_file)
        time.sleep(2)
        convert_wav_to_wav()
        time.sleep(1)
        transcribe_audio()
        time.sleep(.5)

async def main():
    await convert(TEMP_AUDIO_FILE)      

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(str(sys.argv))
        print("Usage: python get_live_data.py [YouTube_link]")
        sys.exit(1)
    print("get_live_data.py is working . . .")
    yt_link = sys.argv[1]

    asyncio.run(main())