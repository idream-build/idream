from setuptools import setup

setup(
    name='idream',
    version='0.1.1',
    maintainer='Eric Conlon',
    maintainer_email='ejconlon@gmail.com',
    packages=['idream'],
    url='https://github.com/ejconlon/idream',
    description='A simple build system for Idris',
    entry_points={
        'console_scripts': ['idream = idream.__main__:main']
    },
    package_data={
        'idream': ['schemas/*.json'],
    },
    install_requires=[
        'jsonschema'
    ]
)
