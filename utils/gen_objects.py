#!/usr/bin/env python3
"""Generate scratch/objects.json — an object-heavy ~2 MB benchmark file."""

import json
import math
import random

random.seed(42)

THEMES = ["dark", "light", "system", "high-contrast"]
LANGS = ["en", "fr", "de", "es", "pt", "ja", "zh"]
CITIES = ["Halifax", "Toronto", "Vancouver", "Montreal", "Calgary", "Ottawa", "Winnipeg"]
REGIONS = ["NS", "ON", "BC", "QC", "AB", "ON", "MB"]
STREETS = ["Main St", "Oak Ave", "Maple Dr", "Cedar Blvd", "Pine Rd", "Elm Way"]
TAGS = ["alpha", "beta", "gamma", "delta", "premium", "trial", "vip", "basic"]

N = 1800

users = {}
for i in range(N):
    uid = f"user_{i:04d}"
    lat = round(random.uniform(43.0, 60.0), 5)
    lon = round(random.uniform(-130.0, -60.0), 5)
    city_idx = random.randrange(len(CITIES))
    street_num = random.randint(1, 9999)
    tag_sample = random.sample(TAGS, k=random.randint(1, 3))
    first_year = random.randint(2018, 2022)
    first_month = random.randint(1, 12)
    first_day = random.randint(1, 28)
    last_year = random.randint(2023, 2025)
    last_month = random.randint(1, 12)
    last_day = random.randint(1, 28)

    extra_fields = {
        f"field_{j:02d}": f"val_{i:04d}_{j:02d}"
        for j in range(10)
    }

    user = {
        "id": i,
        "name": uid,
        "active": bool(random.getrandbits(1)),
        "profile": {
            "email": f"{uid}@example.com",
            "age": random.randint(18, 65),
            "bio": f"Description for user {i:04d}",
            "preferences": {
                "theme": random.choice(THEMES),
                "lang": random.choice(LANGS),
                "notifications": {
                    "email": bool(random.getrandbits(1)),
                    "sms": bool(random.getrandbits(1)),
                    "push": bool(random.getrandbits(1)),
                },
            },
        },
        "location": {
            "lat": lat,
            "lon": lon,
            "address": {
                "street": f"{street_num} {random.choice(STREETS)}",
                "city": CITIES[city_idx],
                "region": REGIONS[city_idx],
                "postal": f"{'ABCDEFGHIJKLMNOPQRSTUVWXYZ'[random.randrange(26)]}"
                          f"{random.randint(0,9)}"
                          f"{'ABCDEFGHIJKLMNOPQRSTUVWXYZ'[random.randrange(26)]}"
                          f"{random.randint(0,9)}"
                          f"{'ABCDEFGHIJKLMNOPQRSTUVWXYZ'[random.randrange(26)]}"
                          f"{random.randint(0,9)}",
            },
        },
        "metrics": {
            "logins": random.randint(1, 500),
            "score": round(random.uniform(0.0, 100.0), 1),
            "tags": tag_sample,
            "history": {
                "first_login": f"{first_year}-{first_month:02d}-{first_day:02d}",
                "last_login": f"{last_year}-{last_month:02d}-{last_day:02d}",
                "sessions": random.randint(1, 200),
            },
        },
        **extra_fields,
    }
    users[uid] = user

output_path = "scratch/objects.json"
with open(output_path, "w") as f:
    json.dump(users, f, indent=2)

import os
size = os.path.getsize(output_path)
print(f"Written {output_path}: {size / 1024 / 1024:.2f} MB, {N} users")
